# simple signature smart contract for adding
from pyteal import *
import base64

int my_add()
#@ensures \result > 0;
{
  int_20 = Int(20);
  int_15 = Int(15);
  
  #int add_expr = Add(int_20, int_15)

  #return add_expr
  int add_expr = int_20 + int_15;
  return add_expr;
}

int main()
#@ requires true;
#@ ensures true;
{
  my_add();
  return 0;
}