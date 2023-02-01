
void foreachFile(char* ext, void (*func)(char* filename, char* extension));

void startPerf();
i64 endPerf();

void* vmem_reserve(u32 size);
void vmem_release(void* address);
void vmem_commit(void* address, u32 size);
void vmem_decommit(void* address, u32 size);