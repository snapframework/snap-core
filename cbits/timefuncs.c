#define _XOPEN_SOURCE
#include <time.h>
#include <locale.h>


void set_c_locale() {
    setlocale(LC_TIME, "C");
}


time_t c_parse_http_time(char* s) {
    struct tm dest;
    strptime(s, "%a, %d %b %Y %H:%M:%S GMT", &dest);
    return mktime(&dest);
}

void c_format_http_time(time_t src, char* dest) {
    struct tm t;
    gmtime_r(&src, &t);
    strftime(dest, 40, "%a, %d %b %Y %H:%M:%S GMT", &t);
}
