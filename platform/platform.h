

typedef struct FileInfo {
    char* path;
    char* name;
    u64 size;

    u64 creation_time, last_access_time, last_write_time;
} FileInfo;

typedef void (*enumerate_files_callback)(FileInfo info, void* user_data);

void enumerate_files(char* path, enumerate_files_callback callback, void* user_data, bool recursive);

void startPerf();
i64 endPerf();

void* vmem_reserve(u64 size);
void vmem_release(void* address);
void vmem_commit(void* address, u64 size);
void vmem_decommit(void* address, u64 size);