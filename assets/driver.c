#include <stdio.h>

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2
#define empty_list   0x2F

extern int scheme_entry();

int main(int argc, char *argv[] ){
  int val = scheme_entry();

  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  } else if (val == empty_list) {
    printf("()\n");
  } else {
    printf("value error\n");
  }

  return 0;
}
