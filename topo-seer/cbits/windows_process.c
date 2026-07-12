#define WIN32_LEAN_AND_MEAN
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wpragma-pack"
#endif
#include <windows.h>
#ifdef __clang__
#pragma clang diagnostic pop
#endif

static void close_if_valid(HANDLE handle)
{
    if (handle != NULL && handle != INVALID_HANDLE_VALUE) {
        CloseHandle(handle);
    }
}

/*
 * Create the plugin suspended, with only explicit standard handles inherited,
 * assign it to the already-configured Job, and only then permit plugin code to
 * execute. On failure the suspended process and every handle created here are
 * closed before returning.
 */
DWORD topo_create_assigned_process_w(
    const wchar_t *application,
    wchar_t *command_line,
    const wchar_t *cwd,
    const void *environment,
    HANDLE job,
    int force_assignment_failure,
    HANDLE *out_process,
    DWORD *out_pid)
{
    STARTUPINFOEXW startup;
    PROCESS_INFORMATION process;
    SECURITY_ATTRIBUTES inheritable;
    HANDLE standard_handles[3];
    HANDLE parent_stderr;
    SIZE_T attribute_size = 0;
    DWORD error = ERROR_SUCCESS;
    BOOL created;

    ZeroMemory(&startup, sizeof(startup));
    ZeroMemory(&process, sizeof(process));
    ZeroMemory(&inheritable, sizeof(inheritable));
    ZeroMemory(standard_handles, sizeof(standard_handles));
    *out_process = NULL;
    *out_pid = 0;

    inheritable.nLength = sizeof(inheritable);
    inheritable.bInheritHandle = TRUE;
    standard_handles[0] = CreateFileW(
        L"NUL", GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
        &inheritable, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    standard_handles[1] = CreateFileW(
        L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
        &inheritable, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    parent_stderr = GetStdHandle(STD_ERROR_HANDLE);
    if (parent_stderr != NULL && parent_stderr != INVALID_HANDLE_VALUE &&
        DuplicateHandle(
            GetCurrentProcess(), parent_stderr,
            GetCurrentProcess(), &standard_handles[2],
            0, TRUE, DUPLICATE_SAME_ACCESS)) {
        /* duplicated stderr is ready */
    } else {
        standard_handles[2] = CreateFileW(
            L"NUL", GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE,
            &inheritable, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
    }
    if (standard_handles[0] == INVALID_HANDLE_VALUE ||
        standard_handles[1] == INVALID_HANDLE_VALUE ||
        standard_handles[2] == INVALID_HANDLE_VALUE ||
        standard_handles[2] == NULL) {
        error = GetLastError();
        goto cleanup_standard_handles;
    }

    InitializeProcThreadAttributeList(NULL, 1, 0, &attribute_size);
    startup.lpAttributeList = (LPPROC_THREAD_ATTRIBUTE_LIST)
        HeapAlloc(GetProcessHeap(), 0, attribute_size);
    if (startup.lpAttributeList == NULL) {
        error = ERROR_NOT_ENOUGH_MEMORY;
        goto cleanup_standard_handles;
    }
    if (!InitializeProcThreadAttributeList(
            startup.lpAttributeList, 1, 0, &attribute_size)) {
        error = GetLastError();
        goto cleanup_attributes;
    }
    if (!UpdateProcThreadAttribute(
            startup.lpAttributeList,
            0,
            PROC_THREAD_ATTRIBUTE_HANDLE_LIST,
            standard_handles,
            sizeof(standard_handles),
            NULL,
            NULL)) {
        error = GetLastError();
        goto cleanup_initialized_attributes;
    }

    startup.StartupInfo.cb = sizeof(startup);
    startup.StartupInfo.dwFlags = STARTF_USESTDHANDLES;
    startup.StartupInfo.hStdInput = standard_handles[0];
    startup.StartupInfo.hStdOutput = standard_handles[1];
    startup.StartupInfo.hStdError = standard_handles[2];
    created = CreateProcessW(
        application,
        command_line,
        NULL,
        NULL,
        TRUE,
        CREATE_SUSPENDED | CREATE_UNICODE_ENVIRONMENT |
            EXTENDED_STARTUPINFO_PRESENT,
        (void *)environment,
        cwd,
        &startup.StartupInfo,
        &process);
    if (!created) {
        error = GetLastError();
    }

    DeleteProcThreadAttributeList(startup.lpAttributeList);
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
    startup.lpAttributeList = NULL;
    close_if_valid(standard_handles[0]);
    close_if_valid(standard_handles[1]);
    close_if_valid(standard_handles[2]);

    if (!created) {
        return error;
    }

    if (force_assignment_failure ||
        !AssignProcessToJobObject(job, process.hProcess)) {
        error = force_assignment_failure ? ERROR_ACCESS_DENIED : GetLastError();
        TerminateProcess(process.hProcess, 1);
        WaitForSingleObject(process.hProcess, INFINITE);
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
        return error;
    }

    if (ResumeThread(process.hThread) == (DWORD)-1) {
        error = GetLastError();
        TerminateJobObject(job, 1);
        WaitForSingleObject(process.hProcess, INFINITE);
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
        return error;
    }

    CloseHandle(process.hThread);
    *out_process = process.hProcess;
    *out_pid = process.dwProcessId;
    return ERROR_SUCCESS;

cleanup_initialized_attributes:
    DeleteProcThreadAttributeList(startup.lpAttributeList);
cleanup_attributes:
    HeapFree(GetProcessHeap(), 0, startup.lpAttributeList);
cleanup_standard_handles:
    close_if_valid(standard_handles[0]);
    close_if_valid(standard_handles[1]);
    close_if_valid(standard_handles[2]);
    return error;
}
