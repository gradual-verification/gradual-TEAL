struct Node {
  int value;
  struct Node* next;
};

"""@
predicate wrappedAcc(struct Node *node) = ? && acc(node->value) && acc(node->next);
@"""

int sumNodes(struct Node* a, struct Node* b, struct Node* c)
#@requires wrappedAcc(a) && wrappedAcc(b) && wrappedAcc(c);
#@ensures wrappedAcc(a) && wrappedAcc(b) && wrappedAcc(c);
{
    #@unfold wrappedAcc(a);
    #@unfold wrappedAcc(b);
    #@unfold wrappedAcc(c);
    int sum = a->value + b->value + c->value;
    #@fold wrappedAcc(a);
    #@fold wrappedAcc(b);
    #@fold wrappedAcc(c);
    return sum;
}

int main()
{
  struct Node* a = alloc(struct Node);
  struct Node* b = alloc(struct Node);
  struct Node* c = alloc(struct Node);
  a->next = NULL;
  b->next = NULL;
  c->next = NULL;
  #@assert acc(a->value) && acc(a->next) && acc(b->value) && acc(b->next) && acc(c->value) && acc(c->next);
  #@fold wrappedAcc(a);
  #@fold wrappedAcc(b);
  #@fold wrappedAcc(c);
  return sumNodes(a, b, c);
}

# Test