struct Student {
  char *name;
  int age;
  float gpa;
};

int main() {
  struct Student student = {
    .name = "John Doe",
    .age = 20,
    .gpa = 9.75,
  };
  
  return 0;
}
