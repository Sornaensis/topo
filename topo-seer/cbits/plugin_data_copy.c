/*
 * Handle-relative, no-follow primitives for plugin data persistence.
 *
 * Source entries are opened relative to an already-open directory handle, so
 * renaming or replacing any source pathname cannot redirect the traversal.
 * Reparse points/symbolic links are never followed.
 */

#if !defined(_WIN32) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include <errno.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)

#define WIN32_LEAN_AND_MEAN
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpragma-pack"
#pragma clang diagnostic ignored "-Winconsistent-dllimport"
#endif
#include <windows.h>
#include <winternl.h>
#ifdef __clang__
#pragma clang diagnostic pop
#endif

#ifndef FILE_OPEN
#define FILE_OPEN 0x00000001UL
#endif
#ifndef FILE_CREATE
#define FILE_CREATE 0x00000002UL
#endif
#ifndef FILE_OPEN_IF
#define FILE_OPEN_IF 0x00000003UL
#endif
#ifndef FILE_DIRECTORY_FILE
#define FILE_DIRECTORY_FILE 0x00000001UL
#endif
#ifndef FILE_NON_DIRECTORY_FILE
#define FILE_NON_DIRECTORY_FILE 0x00000040UL
#endif
#ifndef FILE_OPEN_REPARSE_POINT
#define FILE_OPEN_REPARSE_POINT 0x00200000UL
#endif
#ifndef FILE_OPEN_FOR_BACKUP_INTENT
#define FILE_OPEN_FOR_BACKUP_INTENT 0x00004000UL
#endif
#ifndef FILE_SYNCHRONOUS_IO_NONALERT
#define FILE_SYNCHRONOUS_IO_NONALERT 0x00000020UL
#endif
#ifndef OBJ_CASE_INSENSITIVE
#define OBJ_CASE_INSENSITIVE 0x00000040UL
#endif
#ifndef OBJ_DONT_REPARSE
#define OBJ_DONT_REPARSE 0x00001000UL
#endif

typedef NTSTATUS (NTAPI *topo_nt_create_file_fn)(
    PHANDLE,
    ACCESS_MASK,
    POBJECT_ATTRIBUTES,
    PIO_STATUS_BLOCK,
    PLARGE_INTEGER,
    ULONG,
    ULONG,
    ULONG,
    ULONG,
    PVOID,
    ULONG);

typedef ULONG (WINAPI *topo_rtl_status_to_dos_error_fn)(NTSTATUS);

static void topo_set_windows_error(DWORD error_code) {
    switch (error_code) {
        case ERROR_FILE_NOT_FOUND:
        case ERROR_PATH_NOT_FOUND:
            errno = ENOENT;
            break;
        case ERROR_ACCESS_DENIED:
        case ERROR_SHARING_VIOLATION:
            errno = EACCES;
            break;
        case ERROR_ALREADY_EXISTS:
        case ERROR_FILE_EXISTS:
            errno = EEXIST;
            break;
        case ERROR_NOT_ENOUGH_MEMORY:
        case ERROR_OUTOFMEMORY:
            errno = ENOMEM;
            break;
        case ERROR_FILENAME_EXCED_RANGE:
            errno = ENAMETOOLONG;
            break;
        case ERROR_INVALID_NAME:
        case ERROR_INVALID_PARAMETER:
            errno = EINVAL;
            break;
        default:
            errno = EIO;
            break;
    }
}

static void topo_set_nt_error(NTSTATUS status) {
    HMODULE ntdll = GetModuleHandleW(L"ntdll.dll");
    topo_rtl_status_to_dos_error_fn convert = NULL;
    if (ntdll != NULL) {
        convert = (topo_rtl_status_to_dos_error_fn)
            GetProcAddress(ntdll, "RtlNtStatusToDosError");
    }
    topo_set_windows_error(convert == NULL ? ERROR_GEN_FAILURE : convert(status));
}

static int topo_valid_component(const char *name) {
    return name != NULL
        && name[0] != '\0'
        && strcmp(name, ".") != 0
        && strcmp(name, "..") != 0
        && strchr(name, '/') == NULL
        && strchr(name, '\\') == NULL;
}

intptr_t topo_plugin_open_root(const wchar_t *path) {
    HANDLE handle = CreateFileW(
        path,
        GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        NULL,
        OPEN_EXISTING,
        FILE_FLAG_BACKUP_SEMANTICS | FILE_FLAG_OPEN_REPARSE_POINT,
        NULL);
    if (handle == INVALID_HANDLE_VALUE) {
        topo_set_windows_error(GetLastError());
        return (intptr_t)-1;
    }
    return (intptr_t)handle;
}

static intptr_t topo_plugin_nt_open_child(
        intptr_t parent_value,
        const char *name,
        ACCESS_MASK desired_access,
        ULONG create_disposition,
        ULONG create_options) {
    HANDLE parent = (HANDLE)parent_value;
    HMODULE ntdll;
    topo_nt_create_file_fn nt_create_file;
    int wide_length;
    wchar_t *wide_name;
    UNICODE_STRING unicode_name;
    OBJECT_ATTRIBUTES attributes;
    IO_STATUS_BLOCK io_status;
    HANDLE child = INVALID_HANDLE_VALUE;
    NTSTATUS status;

    if (!topo_valid_component(name)) {
        errno = EINVAL;
        return (intptr_t)-1;
    }

    wide_length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, name, -1, NULL, 0);
    if (wide_length <= 0) {
        topo_set_windows_error(GetLastError());
        return (intptr_t)-1;
    }
    wide_name = (wchar_t *)malloc((size_t)wide_length * sizeof(wchar_t));
    if (wide_name == NULL) {
        errno = ENOMEM;
        return (intptr_t)-1;
    }
    if (MultiByteToWideChar(
            CP_UTF8, MB_ERR_INVALID_CHARS, name, -1, wide_name, wide_length) <= 0) {
        DWORD error_code = GetLastError();
        free(wide_name);
        topo_set_windows_error(error_code);
        return (intptr_t)-1;
    }

    ntdll = GetModuleHandleW(L"ntdll.dll");
    nt_create_file = ntdll == NULL ? NULL :
        (topo_nt_create_file_fn)GetProcAddress(ntdll, "NtCreateFile");
    if (nt_create_file == NULL) {
        free(wide_name);
        errno = ENOSYS;
        return (intptr_t)-1;
    }

    unicode_name.Buffer = wide_name;
    unicode_name.Length = (USHORT)((wide_length - 1) * sizeof(wchar_t));
    unicode_name.MaximumLength = (USHORT)(wide_length * sizeof(wchar_t));
    InitializeObjectAttributes(
        &attributes,
        &unicode_name,
        OBJ_CASE_INSENSITIVE | OBJ_DONT_REPARSE,
        parent,
        NULL);

    status = nt_create_file(
        &child,
        desired_access,
        &attributes,
        &io_status,
        NULL,
        FILE_ATTRIBUTE_NORMAL,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        create_disposition,
        FILE_SYNCHRONOUS_IO_NONALERT
            | FILE_OPEN_REPARSE_POINT
            | FILE_OPEN_FOR_BACKUP_INTENT
            | create_options,
        NULL,
        0);
    free(wide_name);

    if (status < 0) {
        topo_set_nt_error(status);
        return (intptr_t)-1;
    }
    return (intptr_t)child;
}

intptr_t topo_plugin_open_child(intptr_t parent_value, const char *name) {
    return topo_plugin_nt_open_child(
        parent_value,
        name,
        FILE_READ_DATA | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
        FILE_OPEN,
        0);
}

intptr_t topo_plugin_create_directory(intptr_t parent_value, const char *name) {
    return topo_plugin_nt_open_child(
        parent_value,
        name,
        FILE_LIST_DIRECTORY | FILE_ADD_FILE | FILE_ADD_SUBDIRECTORY
            | FILE_TRAVERSE | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
        FILE_OPEN_IF,
        FILE_DIRECTORY_FILE);
}

intptr_t topo_plugin_create_file(intptr_t parent_value, const char *name) {
    return topo_plugin_nt_open_child(
        parent_value,
        name,
        FILE_WRITE_DATA | FILE_READ_ATTRIBUTES | SYNCHRONIZE,
        FILE_CREATE,
        FILE_NON_DIRECTORY_FILE);
}

typedef struct topo_plugin_dir_enum {
    HANDLE directory;
    unsigned char buffer[65536];
    DWORD offset;
    int restart;
    int need_buffer;
} topo_plugin_dir_enum;

intptr_t topo_plugin_dir_enum_open(intptr_t directory_value) {
    topo_plugin_dir_enum *enumeration =
        (topo_plugin_dir_enum *)calloc(1, sizeof(topo_plugin_dir_enum));
    if (enumeration == NULL) {
        errno = ENOMEM;
        return (intptr_t)-1;
    }
    enumeration->directory = (HANDLE)directory_value;
    enumeration->restart = 1;
    enumeration->need_buffer = 1;
    return (intptr_t)enumeration;
}

int topo_plugin_dir_enum_next(intptr_t enumeration_value, char *output, unsigned int output_size) {
    topo_plugin_dir_enum *enumeration = (topo_plugin_dir_enum *)enumeration_value;

    if (enumeration == NULL || output == NULL || output_size == 0) {
        errno = EINVAL;
        return -1;
    }

    for (;;) {
        FILE_ID_BOTH_DIR_INFO *entry;
        int utf8_length;

        if (enumeration->need_buffer) {
            FILE_INFO_BY_HANDLE_CLASS info_class = enumeration->restart
                ? FileIdBothDirectoryRestartInfo
                : FileIdBothDirectoryInfo;
            if (!GetFileInformationByHandleEx(
                    enumeration->directory,
                    info_class,
                    enumeration->buffer,
                    (DWORD)sizeof(enumeration->buffer))) {
                DWORD error_code = GetLastError();
                if (error_code == ERROR_NO_MORE_FILES) {
                    return 0;
                }
                topo_set_windows_error(error_code);
                return -1;
            }
            enumeration->restart = 0;
            enumeration->need_buffer = 0;
            enumeration->offset = 0;
        }

        entry = (FILE_ID_BOTH_DIR_INFO *)(enumeration->buffer + enumeration->offset);
        if (entry->NextEntryOffset == 0) {
            enumeration->need_buffer = 1;
        } else {
            enumeration->offset += entry->NextEntryOffset;
        }

        if ((entry->FileNameLength == sizeof(wchar_t) && entry->FileName[0] == L'.')
            || (entry->FileNameLength == 2 * sizeof(wchar_t)
                && entry->FileName[0] == L'.'
                && entry->FileName[1] == L'.')) {
            continue;
        }

        utf8_length = WideCharToMultiByte(
            CP_UTF8,
            WC_ERR_INVALID_CHARS,
            entry->FileName,
            (int)(entry->FileNameLength / sizeof(wchar_t)),
            NULL,
            0,
            NULL,
            NULL);
        if (utf8_length <= 0) {
            topo_set_windows_error(GetLastError());
            return -1;
        }
        if ((size_t)utf8_length + 1 > output_size) {
            errno = ENAMETOOLONG;
            return -1;
        }
        if (WideCharToMultiByte(
                CP_UTF8,
                WC_ERR_INVALID_CHARS,
                entry->FileName,
                (int)(entry->FileNameLength / sizeof(wchar_t)),
                output,
                utf8_length,
                NULL,
                NULL) != utf8_length) {
            topo_set_windows_error(GetLastError());
            return -1;
        }
        output[utf8_length] = '\0';
        return 1;
    }
}

void topo_plugin_dir_enum_close(intptr_t enumeration_value) {
    free((void *)enumeration_value);
}

int topo_plugin_handle_type(intptr_t handle_value) {
    FILE_ATTRIBUTE_TAG_INFO info;
    if (!GetFileInformationByHandleEx(
            (HANDLE)handle_value,
            FileAttributeTagInfo,
            &info,
            sizeof(info))) {
        topo_set_windows_error(GetLastError());
        return -1;
    }
    if ((info.FileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) != 0) {
        return 3;
    }
    if ((info.FileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0) {
        return 2;
    }
    return 1;
}

int topo_plugin_read_handle(intptr_t handle_value, void *buffer, unsigned int size) {
    DWORD bytes_read = 0;
    if (!ReadFile((HANDLE)handle_value, buffer, size, &bytes_read, NULL)) {
        topo_set_windows_error(GetLastError());
        return -1;
    }
    return (int)bytes_read;
}

int topo_plugin_write_handle(intptr_t handle_value, const void *buffer, unsigned int size) {
    DWORD bytes_written = 0;
    if (!WriteFile((HANDLE)handle_value, buffer, size, &bytes_written, NULL)) {
        topo_set_windows_error(GetLastError());
        return -1;
    }
    return (int)bytes_written;
}

int topo_plugin_close_handle(intptr_t handle_value) {
    if (!CloseHandle((HANDLE)handle_value)) {
        topo_set_windows_error(GetLastError());
        return -1;
    }
    return 0;
}

#else

#include <dirent.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
#ifndef O_DIRECTORY
#define O_DIRECTORY 0
#endif
#ifndef O_NOFOLLOW
#error "Plugin data persistence requires O_NOFOLLOW"
#endif

static int topo_valid_component(const char *name) {
    return name != NULL
        && name[0] != '\0'
        && strcmp(name, ".") != 0
        && strcmp(name, "..") != 0
        && strchr(name, '/') == NULL;
}

intptr_t topo_plugin_open_root(const char *path) {
    int descriptor;
    do {
        descriptor = open(
            path,
            O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK);
    } while (descriptor < 0 && errno == EINTR);
    return (intptr_t)descriptor;
}

intptr_t topo_plugin_open_child(intptr_t parent_value, const char *name) {
    int descriptor;
    if (!topo_valid_component(name)) {
        errno = EINVAL;
        return (intptr_t)-1;
    }
    do {
        descriptor = openat(
            (int)parent_value,
            name,
            O_RDONLY | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK);
    } while (descriptor < 0 && errno == EINTR);
    return (intptr_t)descriptor;
}

intptr_t topo_plugin_create_directory(intptr_t parent_value, const char *name) {
    int result;
    int descriptor;
    if (!topo_valid_component(name)) {
        errno = EINVAL;
        return (intptr_t)-1;
    }
    do {
        result = mkdirat((int)parent_value, name, 0777);
    } while (result < 0 && errno == EINTR);
    if (result < 0 && errno != EEXIST) {
        return (intptr_t)-1;
    }
    do {
        descriptor = openat(
            (int)parent_value,
            name,
            O_RDONLY | O_DIRECTORY | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK);
    } while (descriptor < 0 && errno == EINTR);
    return (intptr_t)descriptor;
}

intptr_t topo_plugin_create_file(intptr_t parent_value, const char *name) {
    int descriptor;
    if (!topo_valid_component(name)) {
        errno = EINVAL;
        return (intptr_t)-1;
    }
    do {
        descriptor = openat(
            (int)parent_value,
            name,
            O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW | O_NONBLOCK,
            0666);
    } while (descriptor < 0 && errno == EINTR);
    return (intptr_t)descriptor;
}

typedef struct topo_plugin_dir_enum {
    DIR *directory;
} topo_plugin_dir_enum;

intptr_t topo_plugin_dir_enum_open(intptr_t directory_value) {
    int duplicate;
    DIR *directory;
    topo_plugin_dir_enum *enumeration;

    do {
        duplicate = dup((int)directory_value);
    } while (duplicate < 0 && errno == EINTR);
    if (duplicate < 0) {
        return (intptr_t)-1;
    }
    directory = fdopendir(duplicate);
    if (directory == NULL) {
        int saved_errno = errno;
        close(duplicate);
        errno = saved_errno;
        return (intptr_t)-1;
    }
    enumeration = (topo_plugin_dir_enum *)malloc(sizeof(topo_plugin_dir_enum));
    if (enumeration == NULL) {
        int saved_errno = errno;
        closedir(directory);
        errno = saved_errno == 0 ? ENOMEM : saved_errno;
        return (intptr_t)-1;
    }
    enumeration->directory = directory;
    return (intptr_t)enumeration;
}

int topo_plugin_dir_enum_next(intptr_t enumeration_value, char *output, unsigned int output_size) {
    topo_plugin_dir_enum *enumeration = (topo_plugin_dir_enum *)enumeration_value;
    struct dirent *entry;

    if (enumeration == NULL || output == NULL || output_size == 0) {
        errno = EINVAL;
        return -1;
    }

    for (;;) {
        size_t length;
        errno = 0;
        entry = readdir(enumeration->directory);
        if (entry == NULL) {
            return errno == 0 ? 0 : -1;
        }
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }
        length = strlen(entry->d_name);
        if (length + 1 > output_size) {
            errno = ENAMETOOLONG;
            return -1;
        }
        memcpy(output, entry->d_name, length + 1);
        return 1;
    }
}

void topo_plugin_dir_enum_close(intptr_t enumeration_value) {
    topo_plugin_dir_enum *enumeration = (topo_plugin_dir_enum *)enumeration_value;
    if (enumeration != NULL) {
        closedir(enumeration->directory);
        free(enumeration);
    }
}

int topo_plugin_handle_type(intptr_t handle_value) {
    struct stat status;
    if (fstat((int)handle_value, &status) < 0) {
        return -1;
    }
    if (S_ISREG(status.st_mode)) {
        return 1;
    }
    if (S_ISDIR(status.st_mode)) {
        return 2;
    }
    if (S_ISLNK(status.st_mode)) {
        return 3;
    }
    return 4;
}

int topo_plugin_read_handle(intptr_t handle_value, void *buffer, unsigned int size) {
    ssize_t count;
    do {
        count = read((int)handle_value, buffer, size);
    } while (count < 0 && errno == EINTR);
    if (count > INT32_MAX) {
        errno = EOVERFLOW;
        return -1;
    }
    return (int)count;
}

int topo_plugin_write_handle(intptr_t handle_value, const void *buffer, unsigned int size) {
    ssize_t count;
    do {
        count = write((int)handle_value, buffer, size);
    } while (count < 0 && errno == EINTR);
    if (count > INT32_MAX) {
        errno = EOVERFLOW;
        return -1;
    }
    return (int)count;
}

int topo_plugin_close_handle(intptr_t handle_value) {
    int result = close((int)handle_value);
    /* POSIX permits EINTR after the descriptor has already been released. */
    return result < 0 && errno == EINTR ? 0 : result;
}

#endif
