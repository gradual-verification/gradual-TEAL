from pyteal import *
import base64

int add()
{
  int_20 = Int(20);
  int_15 = Int(15);
  
  #add_expr = Add(int_20, int_15)

  #return add_expr
  int add_expr = int_20 + int_15;
  return add_expr;
}

int main()
{
  add();
  return 0;
}