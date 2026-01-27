// Comprehensive Test: Linked List Operations
// Tests: Structs, Pointers, Arrays, Loops, Functions, and strict Type Checking.

struct Node {
    int val;
    Node* next;
};

// Global sentinel to represent "NULL" terminator
// We use this because strict type checking might reject (ptr == 0)
Node* NULL_NODE;

// Initialize the sentinel
void init_globals() {
    NULL_NODE = new Node[1];
}

// Create a new node with a value
Node* create_node(int v) {
    Node* n;
    n = new Node[1];
    n[0].val = v;
    n[0].next = NULL_NODE; // Point to sentinel
    return n;
}

// Append a value to the end of the list
Node* append(Node* head, int v) {
    // If list is empty (pointing to sentinel)
    if (head == NULL_NODE) {
        return create_node(v);
    }
    
    Node* current;
    current = head;
    
    // Traverse to the last node
    while (current[0].next != NULL_NODE) {
        current = current[0].next;
    }
    
    // Link the new node
    current[0].next = create_node(v);
    return head;
}

// Calculate sum of all values in the list
int sum_list(Node* head) {
    int sum;
    sum = 0;
    
    Node* current;
    current = head;
    
    while (current != NULL_NODE) {
        sum = sum + current[0].val;
        current = current[0].next;
    }
    return sum;
}

int main() {
    init_globals();

    Node* list;
    list = NULL_NODE; // Start with empty list
    
    // Build the list: 10 -> 20 -> 30
    list = append(list, 10);
    list = append(list, 20);
    list = append(list, 30);
    
    // Verify result: 10 + 20 + 30 should be 60
    return sum_list(list);
}