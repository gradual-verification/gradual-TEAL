from pyteal import *

# comment: This is a test for the basic pyteal or python arithmetic operations
def arithmetic_operation(quantity: abi.Uint64, quantity2: abi.Uint64):
    #@ requires quantity >= 0;
    int_20 = Int(20)
    int_20_20 = int_20
    int_15 = Int(15)
    int_55 = Int(55)
    
    add_40 = Add(int_20_20, Int(5), int_15)
    div_8 = Div(add_40, Int(5))
    int_0 = div_8 >= int_55
    int_3 = Mod(div_8, Int(5))
    #int_1 = (Int(2) == BitwiseAnd(int_3, Int(6)))
    comp_and = And(add_40, int_55)  
    comp_neq = Neq(quantity, comp_and)
    comp_or = Or(comp_neq, quantity2)

    return comp_or
