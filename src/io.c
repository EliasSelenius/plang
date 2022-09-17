
char* fileread(const char* filename, u32* strLength) {
    FILE* file;
    if ( fopen_s(&file, filename, "r") ) {
        printf("Could not read file: %s\n", filename);
        return null;
    }

    fseek(file, 0, SEEK_END);
    int bufferLength = ftell(file) + 1; // CRLF gurantees there is enough space, but when the file is not in CRLF we add one to get space for null termination
    rewind(file);

    char* res = calloc(bufferLength, 1);
    u64 len = fread(res, 1, bufferLength, file);
    res[len] = '\0';
    *strLength = (u32)len;

    fclose(file);

    return res;
}

void filewrite(const char* filename, char* content) {
    FILE* file;
    if ( fopen_s(&file, filename, "w") ) {
        printf("Could not write to file: %s\n", filename);
        return;
    }

    fprintf(file, "%s", content);

    fclose(file);
}