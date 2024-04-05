#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LENGTH 10

typedef struct Node {
    void *element;      // an element (you can make this any type, but if you want your list to be general, make it void *)
    struct Node *next;  // a pointer to the next node of the list
    struct Node *prev;  // a pointer to the previous node of the list
} Node;

typedef struct List {
    Node *head;         // a pointer to the first node of the list
    Node *tail;         // a pointer to the last node of the list
} List;

// Function prototypes
void add_to_front(void *element, List *list);
void add_to_back(void *element, List *list);
void* remove_from_front(List *list);
void* remove_from_back(List *list);
void transfer(void *array1[], void *array2[], int length, void (*insert)(void *, List *), void *(*remove)(List *));
List* create_list();
void free_list(List *list);

// Create a main function that tests your transfer function. Create two arrays,
// one filled with appropriate data and the other empty. Call the transfer
// function on the two arrays and the methods add_to_front and
// remove_from_front. Print the contents of the second array. If correct, the
// contents should be reversed. Repeat this process 3 more times so you can
// test with all combinations of the add and remove functions.
int main() {
    char* data[] = {"Azzaro", "Creed", "Dior", "Jean Paul Gaultier", "Paco Rabanne", "Penhaligon's", "Tom Ford", "Valentino", "Viktor & Rolf", "Yves Saint Laurent"};

    int colWidth = 80;

    // Using a single loop with a switch case for the four combinations
    for (int count = 0; count < 4; ++count) {

        for (int dash_count = 0; dash_count < colWidth; dash_count++) {
            printf("=");
        }
        puts("");

        void *array1[LENGTH];
        for (int k = 0; k < LENGTH; ++k) {
            array1[k] = data[k];
        }

        void *array2[LENGTH];
        for (int k = 0; k < LENGTH; k++) {
            array2[k] = NULL;
        }

        switch (count) {
            case 0:
                printf("Operation #%d: Adding elements from 'array1' to the front of the list, and then\n", count + 1);
                printf("removing them from the front of the list to place in 'array2' reverses the\n");
                printf("order of elements.\n\n");
                transfer(array1, array2, LENGTH, add_to_front, remove_from_front);
                break;
            case 1:
                printf("Operation #%d: Adding elements from 'array1' to the front of the list, and then\n", count + 1);
                printf("removing them from the back of the list to place in 'array2' maintains the\n");
                printf("original order of elements.\n\n");
                transfer(array1, array2, LENGTH, add_to_front, remove_from_back);
                break;
            case 2:
                printf("Operation #%d: Adding elements from 'array1' to the back of the list, and then\n", count + 1);
                printf("removing them from the front of the list to place in 'array2' maintains the\n");
                printf("original order of elements.\n\n");
                transfer(array1, array2, LENGTH, add_to_back, remove_from_front);
                break;
            case 3:
                printf("Operation #%d: Adding elements from 'array1' to the back of the list, and then\n", count + 1);
                printf("removing them from the back of the list to place in 'array2' reverses the\n");
                printf("order of elements.\n\n");
                transfer(array1, array2, LENGTH, add_to_back, remove_from_back);
                break;
        }

        puts("");
        printf("Here is array2 after the transfer: ");
        int widthTotal = 35; // Starts at 35 to account for "Here is array2 after..."
        for (int k = 0; k < LENGTH; k++) {
            printf("%s", (char*)array2[k]);
            widthTotal += (strlen(array2[k]) + 2);
            if ((widthTotal + strlen(array2[k+1])) > colWidth) {
                if (k < (LENGTH - 1)) {
                    puts(",");
                }
                widthTotal = 0;
            }
            else {
                printf("%s", k == (LENGTH - 1)? "" : ", ");
            }
        }

        printf("\n\n");
        for (int dash_count = 0; dash_count < colWidth; dash_count++) {
            printf("=");
        }
        puts("");
    }

    return 0;
}

List* create_list() {
    List *list = (List *)malloc(sizeof(List));
    list->head = NULL;
    list->tail = NULL;
    return list;
}

void free_list(List *list) {
    Node *current = list->head;
    Node *next;
    while (current != NULL) {
        next = current->next;
        free(current);
        current = next;
    }
    free(list);
}

// add_to_front takes an element and a list and it adds a new node containing
// the element to the front of the list
void add_to_front(void *element, List *list) {
    Node *new_node = (Node *)malloc(sizeof(Node));
    new_node->element = element;
    new_node->next = list->head;
    new_node->prev = NULL;
    if (list->head != NULL) {
        list->head->prev = new_node;
    }
    list->head = new_node;
    if (list->tail == NULL) {
        list->tail = new_node;
    }
}

// add_to_back takes an element and a list and it adds a new node containing
// the element to the back of the list
void add_to_back(void *element, List *list) {
    Node *new_node = (Node *)malloc(sizeof(Node));
    new_node->element = element;
    new_node->prev = list->tail;
    new_node->next = NULL;
    if (list->tail != NULL) {
        list->tail->next = new_node;
    }
    list->tail = new_node;
    if (list->head == NULL) {
        list->head = new_node;
    }
}

// remove_from_front takes a list and removes (and frees) the node that was at
// the front of the list, and returns the element stored in that node.
void* remove_from_front(List *list) {
    if (list->head == NULL) {
        return NULL;
    }
    Node *old_head = list->head;
    void *element = old_head->element;
    list->head = old_head->next;
    if (list->head != NULL) {
        list->head->prev = NULL;
    } else {
        list->tail = NULL;
    }
    free(old_head);
    return element;
}

// remove_from_back takes a list and removes (and frees) the node that was at
// the back of the list, and returns the element stored in that node.
void* remove_from_back(List *list) {
    if (list->tail == NULL) {
        return NULL;
    }
    Node *old_tail = list->tail;
    void *element = old_tail->element;
    list->tail = old_tail->prev;
    if (list->tail != NULL) {
        list->tail->next = NULL;
    } else {
        list->head = NULL;
    }
    free(old_tail);
    return element;
}


void transfer(void *array1[], void *array2[], int length, void (*insert)(void *, List *), void *(*remove)(List *)) {
    List *list = create_list();

    char* insertAction = (insert == add_to_front) ? "front" : "back";
    char* removeAction = (remove == remove_from_front) ? "front" : "back";

    // Insert each element from array1 into the list
    for (int i = 0; i < length; ++i) {
        insert(array1[i], list);
        printf("Transferred '%s' from array1 to the %s of the list.\n", (char *)array1[i], insertAction);
    }

    puts("");

    // Remove elements from the list and place them into array2
    for (int i = 0; i < length; ++i) {
        array2[i] = remove(list);
        printf("Transferred '%s' from the %s of the list to array2.\n", (char *)array2[i], removeAction);
    }
    free_list(list);
}


