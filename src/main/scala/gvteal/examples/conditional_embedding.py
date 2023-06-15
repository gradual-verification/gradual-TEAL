def test(i):
  #@requires ?;
  #@ensures \result > 0 ? \result < 2 : \result > -2;
  
  # The condition (\result > 0) should be embedded in the if;
  # no temporary variable should be used
  return i

def main():
  return test(1)

# Original C0 Code:
# int test(int input)
#   //@requires ?;
#   //@ensures \result > 0 ? \result < 2 : \result > -2;
# {
#   // The condition (\result > 0) should be embedded in the if;
#   // no temporary variable should be used
#   return input;
# }

# int main()
# {
#   return 0;
# }