#define _GNU_SOURCE

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TOPO_STORAGE_OK 0
#define TOPO_STORAGE_UNSAFE 2
#define TOPO_STORAGE_CONFLICT 3
#define TOPO_STORAGE_UNAVAILABLE 4
#define TOPO_STORAGE_IO 5

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpragma-pack"
#pragma clang diagnostic ignored "-Wmissing-declarations"
#pragma clang diagnostic ignored "-Winconsistent-dllimport"
#endif
#include <windows.h>
#include <winternl.h>
#ifdef __clang__
#pragma clang diagnostic pop
#endif

#ifndef FILE_OPEN_REPARSE_POINT
#define FILE_OPEN_REPARSE_POINT 0x00200000
#endif
#ifndef OBJ_DONT_REPARSE
#define OBJ_DONT_REPARSE 0x00001000L
#endif

struct topo_storage_root { HANDLE handle; };
struct topo_storage_txn {
    HANDLE parent;
    wchar_t *destination;
    unsigned char *bytes;
    size_t length;
};

typedef NTSTATUS (NTAPI *topo_nt_create_file_fn)(
    PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES, PIO_STATUS_BLOCK, PLARGE_INTEGER,
    ULONG, ULONG, ULONG, ULONG, PVOID, ULONG);

static topo_nt_create_file_fn topo_nt_create_file(void) {
    HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
    if (ntdll == NULL) return NULL;
    return (topo_nt_create_file_fn)GetProcAddress(ntdll, "NtCreateFile");
}

static int topo_ntstatus_is(NTSTATUS actual, uint32_t expected) {
    return (uint32_t)actual == expected;
}

static wchar_t *topo_utf8_to_wide(const char *text, int length) {
    int needed;
    wchar_t *wide;
    if (length < 0) length = (int)strlen(text);
    needed = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, length, NULL, 0);
    if (needed <= 0 || needed > 32767) return NULL;
    wide = (wchar_t *)calloc((size_t)needed + 1, sizeof(wchar_t));
    if (wide == NULL) return NULL;
    if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, text, length, wide, needed) != needed) {
        free(wide);
        return NULL;
    }
    return wide;
}

static int topo_safe_directory(HANDLE handle) {
    FILE_ATTRIBUTE_TAG_INFO info;
    if (!GetFileInformationByHandleEx(handle, FileAttributeTagInfo, &info, sizeof(info)))
        return TOPO_STORAGE_UNAVAILABLE;
    if ((info.FileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0)
        return TOPO_STORAGE_UNSAFE;
    if ((info.FileAttributes & FILE_ATTRIBUTE_DIRECTORY) == 0)
        return TOPO_STORAGE_CONFLICT;
    return TOPO_STORAGE_OK;
}

static int topo_open_child_directory(HANDLE parent, const char *name, int length, HANDLE *result) {
    topo_nt_create_file_fn nt_create = topo_nt_create_file();
    wchar_t *wide;
    UNICODE_STRING unicode_name;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK status_block;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    int checked;
    if (nt_create == NULL) return TOPO_STORAGE_UNAVAILABLE;
    wide = topo_utf8_to_wide(name, length);
    if (wide == NULL) return TOPO_STORAGE_IO;
    unicode_name.Buffer = wide;
    unicode_name.Length = (USHORT)(wcslen(wide) * sizeof(wchar_t));
    unicode_name.MaximumLength = unicode_name.Length;
    InitializeObjectAttributes(&attributes, &unicode_name,
        OBJ_CASE_INSENSITIVE | OBJ_DONT_REPARSE, parent, NULL);
    status = nt_create(&child,
        FILE_LIST_DIRECTORY | FILE_ADD_FILE | FILE_ADD_SUBDIRECTORY |
            FILE_READ_ATTRIBUTES | SYNCHRONIZE,
        &attributes, &status_block, NULL, FILE_ATTRIBUTE_DIRECTORY,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        FILE_OPEN_IF,
        FILE_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT | FILE_OPEN_REPARSE_POINT,
        NULL, 0);
    free(wide);
    if (topo_ntstatus_is(status, UINT32_C(0xC0000103)))
        return TOPO_STORAGE_CONFLICT; /* STATUS_NOT_A_DIRECTORY */
    if (topo_ntstatus_is(status, UINT32_C(0xC0000024)) ||
            topo_ntstatus_is(status, UINT32_C(0xC000050B)))
        return TOPO_STORAGE_UNSAFE; /* type mismatch or reparse encountered */
    if (status < 0) return TOPO_STORAGE_UNAVAILABLE;
    checked = topo_safe_directory(child);
    if (checked != TOPO_STORAGE_OK) {
        CloseHandle(child);
        return checked;
    }
    *result = child;
    return TOPO_STORAGE_OK;
}

int topo_storage_root_open(const char *path, struct topo_storage_root **result) {
    wchar_t *wide = topo_utf8_to_wide(path, -1);
    struct topo_storage_root *root;
    HANDLE handle;
    int checked;
    if (wide == NULL) return TOPO_STORAGE_IO;
    handle = CreateFileW(wide,
        FILE_LIST_DIRECTORY | FILE_ADD_FILE | FILE_ADD_SUBDIRECTORY | FILE_READ_ATTRIBUTES,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL, OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
        NULL);
    free(wide);
    if (handle == INVALID_HANDLE_VALUE) return TOPO_STORAGE_UNAVAILABLE;
    checked = topo_safe_directory(handle);
    if (checked != TOPO_STORAGE_OK) {
        CloseHandle(handle);
        return checked;
    }
    root = (struct topo_storage_root *)malloc(sizeof(*root));
    if (root == NULL) {
        CloseHandle(handle);
        return TOPO_STORAGE_IO;
    }
    root->handle = handle;
    *result = root;
    return TOPO_STORAGE_OK;
}

void topo_storage_root_free(struct topo_storage_root *root) {
    if (root != NULL) {
        CloseHandle(root->handle);
        free(root);
    }
}

int topo_storage_begin(struct topo_storage_root *root, const char *path,
        const unsigned char *bytes, size_t length, struct topo_storage_txn **result) {
    const char *segment = path;
    const char *slash;
    HANDLE parent;
    struct topo_storage_txn *txn;
    int status;
    if (root == NULL || path == NULL) return TOPO_STORAGE_UNAVAILABLE;
    if (!DuplicateHandle(GetCurrentProcess(), root->handle, GetCurrentProcess(), &parent,
            0, FALSE, DUPLICATE_SAME_ACCESS))
        return TOPO_STORAGE_UNAVAILABLE;
    slash = strchr(segment, '/');
    while (slash != NULL) {
        HANDLE child;
        status = topo_open_child_directory(parent, segment, (int)(slash - segment), &child);
        CloseHandle(parent);
        if (status != TOPO_STORAGE_OK) return status;
        parent = child;
        segment = slash + 1;
        slash = strchr(segment, '/');
    }
    txn = (struct topo_storage_txn *)calloc(1, sizeof(*txn));
    if (txn == NULL) {
        CloseHandle(parent);
        return TOPO_STORAGE_IO;
    }
    txn->destination = topo_utf8_to_wide(segment, -1);
    txn->bytes = length == 0 ? NULL : (unsigned char *)malloc(length);
    if (txn->destination == NULL || (length != 0 && txn->bytes == NULL)) {
        CloseHandle(parent);
        free(txn->destination);
        free(txn->bytes);
        free(txn);
        return TOPO_STORAGE_IO;
    }
    if (length != 0) memcpy(txn->bytes, bytes, length);
    txn->parent = parent;
    txn->length = length;
    *result = txn;
    return TOPO_STORAGE_OK;
}

int topo_storage_commit(struct topo_storage_txn *txn) {
    topo_nt_create_file_fn nt_create = topo_nt_create_file();
    UNICODE_STRING unicode_name;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK status_block;
    HANDLE destination = INVALID_HANDLE_VALUE;
    NTSTATUS status;
    size_t offset = 0;
    if (txn == NULL || nt_create == NULL) return TOPO_STORAGE_UNAVAILABLE;
    unicode_name.Buffer = txn->destination;
    unicode_name.Length = (USHORT)(wcslen(txn->destination) * sizeof(wchar_t));
    unicode_name.MaximumLength = unicode_name.Length;
    InitializeObjectAttributes(&attributes, &unicode_name,
        OBJ_CASE_INSENSITIVE | OBJ_DONT_REPARSE, txn->parent, NULL);
    status = nt_create(&destination,
        FILE_WRITE_DATA | FILE_WRITE_ATTRIBUTES | DELETE | SYNCHRONIZE,
        &attributes, &status_block, NULL, FILE_ATTRIBUTE_NORMAL,
        0, FILE_CREATE,
        FILE_NON_DIRECTORY_FILE | FILE_SYNCHRONOUS_IO_NONALERT |
            FILE_OPEN_REPARSE_POINT | FILE_DELETE_ON_CLOSE,
        NULL, 0);
    if (topo_ntstatus_is(status, UINT32_C(0xC0000035)) ||
            topo_ntstatus_is(status, UINT32_C(0xC0000024)) ||
            topo_ntstatus_is(status, UINT32_C(0xC00000BA)) ||
            topo_ntstatus_is(status, UINT32_C(0xC0000103)) ||
            topo_ntstatus_is(status, UINT32_C(0xC000050B)))
        return TOPO_STORAGE_CONFLICT;
    if (status < 0) return TOPO_STORAGE_UNAVAILABLE;
    while (offset < txn->length) {
        DWORD chunk = (DWORD)((txn->length - offset) > 0x7ffff000U
            ? 0x7ffff000U : (txn->length - offset));
        DWORD written = 0;
        if (!WriteFile(destination, txn->bytes + offset, chunk, &written, NULL) || written == 0) {
            CloseHandle(destination);
            return TOPO_STORAGE_IO;
        }
        offset += written;
    }
    if (!FlushFileBuffers(destination)) {
        CloseHandle(destination);
        return TOPO_STORAGE_IO;
    }
    {
        FILE_DISPOSITION_INFO disposition;
        disposition.DeleteFile = FALSE;
        if (!SetFileInformationByHandle(destination, FileDispositionInfo,
                &disposition, sizeof(disposition))) {
            CloseHandle(destination);
            return TOPO_STORAGE_IO;
        }
    }
    CloseHandle(destination);
    return TOPO_STORAGE_OK;
}

void topo_storage_txn_free(struct topo_storage_txn *txn) {
    if (txn != NULL) {
        CloseHandle(txn->parent);
        free(txn->destination);
        free(txn->bytes);
        free(txn);
    }
}

#elif defined(__linux__)

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

struct topo_storage_root { int fd; };
struct topo_storage_txn { int parent_fd; int temporary_fd; char *destination; };

static int topo_parent_error(int parent, const char *segment) {
    struct stat info;
    if (fstatat(parent, segment, &info, AT_SYMLINK_NOFOLLOW) == 0) {
        if (S_ISLNK(info.st_mode)) return TOPO_STORAGE_UNSAFE;
        if (!S_ISDIR(info.st_mode)) return TOPO_STORAGE_CONFLICT;
    }
    return TOPO_STORAGE_UNAVAILABLE;
}

int topo_storage_root_open(const char *path, struct topo_storage_root **result) {
    int fd = open(path, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    struct stat info;
    struct topo_storage_root *root;
    if (fd < 0) return errno == ELOOP ? TOPO_STORAGE_UNSAFE : TOPO_STORAGE_UNAVAILABLE;
    if (fstat(fd, &info) != 0 || !S_ISDIR(info.st_mode)) {
        close(fd);
        return TOPO_STORAGE_UNAVAILABLE;
    }
    root = (struct topo_storage_root *)malloc(sizeof(*root));
    if (root == NULL) {
        close(fd);
        return TOPO_STORAGE_IO;
    }
    root->fd = fd;
    *result = root;
    return TOPO_STORAGE_OK;
}

void topo_storage_root_free(struct topo_storage_root *root) {
    if (root != NULL) {
        close(root->fd);
        free(root);
    }
}

static int topo_write_all(int fd, const unsigned char *bytes, size_t length) {
    size_t offset = 0;
    while (offset < length) {
        ssize_t written = write(fd, bytes + offset, length - offset);
        if (written < 0 && errno == EINTR) continue;
        if (written <= 0) return TOPO_STORAGE_IO;
        offset += (size_t)written;
    }
    return fsync(fd) == 0 ? TOPO_STORAGE_OK : TOPO_STORAGE_IO;
}

int topo_storage_begin(struct topo_storage_root *root, const char *path,
        const unsigned char *bytes, size_t length, struct topo_storage_txn **result) {
    char *copy;
    char *segment;
    char *slash;
    int parent;
    int temporary;
    int status;
    struct topo_storage_txn *txn;
    if (root == NULL || path == NULL) return TOPO_STORAGE_UNAVAILABLE;
    parent = fcntl(root->fd, F_DUPFD_CLOEXEC, 0);
    if (parent < 0) return TOPO_STORAGE_UNAVAILABLE;
    copy = strdup(path);
    if (copy == NULL) {
        close(parent);
        return TOPO_STORAGE_IO;
    }
    segment = copy;
    slash = strchr(segment, '/');
    while (slash != NULL) {
        int child;
        *slash = '\0';
        if (mkdirat(parent, segment, 0700) != 0 && errno != EEXIST) {
            status = topo_parent_error(parent, segment);
            close(parent);
            free(copy);
            return status;
        }
        child = openat(parent, segment, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
        if (child < 0) {
            status = topo_parent_error(parent, segment);
            close(parent);
            free(copy);
            return status;
        }
        close(parent);
        parent = child;
        segment = slash + 1;
        slash = strchr(segment, '/');
    }
#ifdef O_TMPFILE
    temporary = openat(parent, ".", O_TMPFILE | O_RDWR | O_CLOEXEC, 0600);
#else
    temporary = -1;
    errno = EOPNOTSUPP;
#endif
    if (temporary < 0) {
        close(parent);
        free(copy);
        return TOPO_STORAGE_UNAVAILABLE;
    }
    status = topo_write_all(temporary, bytes, length);
    if (status != TOPO_STORAGE_OK) {
        close(temporary);
        close(parent);
        free(copy);
        return status;
    }
    txn = (struct topo_storage_txn *)malloc(sizeof(*txn));
    if (txn == NULL) {
        close(temporary);
        close(parent);
        free(copy);
        return TOPO_STORAGE_IO;
    }
    txn->destination = strdup(segment);
    free(copy);
    if (txn->destination == NULL) {
        close(temporary);
        close(parent);
        free(txn);
        return TOPO_STORAGE_IO;
    }
    txn->parent_fd = parent;
    txn->temporary_fd = temporary;
    *result = txn;
    return TOPO_STORAGE_OK;
}

int topo_storage_commit(struct topo_storage_txn *txn) {
    if (txn == NULL) return TOPO_STORAGE_UNAVAILABLE;
    if (linkat(txn->temporary_fd, "", txn->parent_fd, txn->destination, AT_EMPTY_PATH) == 0) {
        (void)fsync(txn->parent_fd);
        return TOPO_STORAGE_OK;
    }
    if (errno == EEXIST) return TOPO_STORAGE_CONFLICT;
    if (errno == EOPNOTSUPP || errno == ENOTSUP || errno == EINVAL || errno == EPERM)
        return TOPO_STORAGE_UNAVAILABLE;
    return TOPO_STORAGE_IO;
}

void topo_storage_txn_free(struct topo_storage_txn *txn) {
    if (txn != NULL) {
        close(txn->temporary_fd);
        close(txn->parent_fd);
        free(txn->destination);
        free(txn);
    }
}

#else

struct topo_storage_root { int unused; };
struct topo_storage_txn { int unused; };

int topo_storage_root_open(const char *path, struct topo_storage_root **result) {
    (void)path; (void)result;
    return TOPO_STORAGE_UNAVAILABLE;
}
void topo_storage_root_free(struct topo_storage_root *root) { (void)root; }
int topo_storage_begin(struct topo_storage_root *root, const char *path,
        const unsigned char *bytes, size_t length, struct topo_storage_txn **result) {
    (void)root; (void)path; (void)bytes; (void)length; (void)result;
    return TOPO_STORAGE_UNAVAILABLE;
}
int topo_storage_commit(struct topo_storage_txn *txn) {
    (void)txn;
    return TOPO_STORAGE_UNAVAILABLE;
}
void topo_storage_txn_free(struct topo_storage_txn *txn) { (void)txn; }

#endif
