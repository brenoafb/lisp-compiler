#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define fixnum_mask  ((int64_t) 3)
#define fixnum_tag   ((int64_t) 0)
#define fixnum_shift ((int64_t) 2)
#define char_mask    ((int64_t) 0xFF)
#define char_tag     ((int64_t) 0x0E)
#define char_shift   ((int64_t) 8)
#define bool_mask    ((int64_t) 0x3F)
#define bool_tag     ((int64_t) 0x1F)
#define bool_shift   ((int64_t) 7)
#define empty_list   ((int64_t) 0x2F)
#define ref_mask     ((int64_t) 0x7)
#define ref_shift    ((int64_t) 3)
#define cons_tag     ((int64_t) 0x1)
#define vec_tag      ((int64_t) 0x2)
#define str_tag      ((int64_t) 0x3)
#define symb_tag     ((int64_t) 0x5)
#define closure_tag  ((int64_t) 0x6)


#define HEAP_SIZE    ((int64_t) 8192)

extern int scheme_entry(void *heap);

int main(int argc, char *argv[] ){
  void *heap = malloc(HEAP_SIZE);
  // printf("heap: 0x%lX\n", heap);
  // scheme_entry receives parameter in register rdi
  int64_t val = scheme_entry(heap);

  // printf("0x%lX\n", val);

  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%ld\n", val >> fixnum_shift);
  } else if ((val & char_mask) == char_tag) {
    printf("%ld\n", val >> char_shift);
  } else if ((val & bool_mask) == bool_tag) {
    char *s = (val >> bool_shift) ? "true" : "false";
    printf("%s\n", s);
  } else if ((val & ref_mask) == cons_tag) {
    printf("<cons>\n");
  } else if ((val & ref_mask) == vec_tag) {
    printf("<vector>\n");
  } else if ((val & ref_mask) == str_tag) {
    printf("<string>\n");
  } else if ((val & ref_mask) == symb_tag) {
    printf("<symbol>\n");
  } else if ((val & ref_mask) == closure_tag) {
    printf("<closure>\n");
  } else if (val == empty_list) {
    printf("()\n");
  } else {
    printf("value error\n");
  }

  free(heap);

  return 0;
}
