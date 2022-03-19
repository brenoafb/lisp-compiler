#include <stdio.h>
#include <stdlib.h>

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2
#define char_mask    0xFF
#define char_tag     0x0E
#define char_shift   8
#define bool_mask    0x3F
#define bool_tag     0x1F
#define bool_shift   7
#define empty_list   0x2F
#define HEAP_SIZE    8192

extern int scheme_entry(void *heap);

int main(int argc, char *argv[] ){
  void *heap = malloc(HEAP_SIZE);
  // scheme_entry receives parameter in register rdi
  int val = scheme_entry(heap);

  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if ((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  } else if ((val & bool_mask) == bool_tag) {
    char *s = (val >> bool_shift) ? "true" : "false";
    printf("%s\n", s);
  } else if (val == empty_list) {
    printf("()\n");
  } else {
    printf("value error\n");
  }

  free(heap);

  return 0;
}
