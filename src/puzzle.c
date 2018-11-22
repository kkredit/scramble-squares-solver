
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

typedef enum edge_e {
    ANT_HEAD        = 1,
    ANT_TAIL        = -1,
    BEETLE_HEAD     = 2,
    BEETLE_TAIL     = -2,
    DRAGONFLY_HEAD  = 3,
    DRAGONFLY_TAIL  = -3,
    MANTIS_HEAD     = 4,
    MANTIS_TAIL     = -4
} edge_t;

typedef edge_t piece_t[4];

/* Piece
 *      edge 1
 *    e       e
 *    d       d
 *    g       g
 *    e       e
 *    4       2
 *      edge 3
 */

const piece_t pieces_set[9] = {
    {DRAGONFLY_TAIL, ANT_HEAD, BEETLE_TAIL, MANTIS_HEAD},
    {DRAGONFLY_TAIL, ANT_TAIL, BEETLE_HEAD, MANTIS_TAIL},
    {DRAGONFLY_TAIL, MANTIS_HEAD, BEETLE_TAIL, ANT_HEAD},
    {DRAGONFLY_TAIL, ANT_HEAD, MANTIS_HEAD, ANT_TAIL},
    {DRAGONFLY_TAIL, ANT_HEAD, BEETLE_HEAD, MANTIS_HEAD},
    {DRAGONFLY_HEAD, BEETLE_TAIL, MANTIS_HEAD, ANT_TAIL},
    {DRAGONFLY_HEAD, MANTIS_TAIL, BEETLE_HEAD, ANT_TAIL},
    {DRAGONFLY_HEAD, MANTIS_HEAD, BEETLE_HEAD, DRAGONFLY_TAIL},
    {BEETLE_TAIL, MANTIS_TAIL, ANT_HEAD, BEETLE_HEAD},
};

/* Board
 *    1 2 3
 *    4 5 6
 *    7 8 9
 */

typedef struct placed_piece_s {
    int         set_index;
    int         rotation;
} placed_piece_t;

typedef placed_piece_t board_t[9];

unsigned long num_boards_checked = 0;

bool board_is_legal(board_t board);
void print_board(board_t board, bool is_solution);
void recursive_check(board_t board);

int main(){
    printf("Working on the solution to the puzzle...\n");
    board_t board = {
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
        {-1, 0},
    };
    recursive_check(board);
    printf("Checked %lu boards\n", num_boards_checked);
    return 0;
}

void recursive_check(board_t board){
    // Each level of recursion adds each possible next piece
    // after adding, check for legal state
    // if legal
    //      if full board
    //          print solution_num
    //      else
    //          recur next level

    // Determine the legal next moves
    bool next_moves[9] = {true, true, true, true, true, true, true, true, true};
    unsigned moves_left = 9;
    for(unsigned i = 0; i < 9; i++){
        if(-1 != board[i].set_index){
            next_moves[board[i].set_index] = false;
            moves_left--;
        }
    }

    // For each possible move
    for(unsigned index = 0; index < 9; index++){
        // If legal
        if(next_moves[index]){
            // For each rotation
            for(unsigned rot_pos = 0; rot_pos < 4; rot_pos++){
                // Add to board, check if legal, follow logic outlined above
                // RETURN ONLY IF IS COMPELTE DEAD END
                board[9 - moves_left].set_index = index;
                board[9 - moves_left].rotation  = rot_pos;
                if(board_is_legal(board)){
                    if(1 == moves_left){
                        print_board(board, true);
                    }
                    else {
                        board_t board_copy;
                        memcpy(board_copy, board, sizeof(board_t));
                        recursive_check(board_copy);
                    }
                }
            }
        }
    }
}

bool board_is_legal(board_t board){
    num_boards_checked++;
    // Vertical edge matching checks
    if(-1 != board[0].set_index
       && -1 != board[1].set_index
       && (pieces_set[board[0].set_index][(1 + board[0].rotation) % 4]
           != -pieces_set[board[1].set_index][(3 + board[1].rotation) % 4])){
        return false;
    }
    if(-1 != board[1].set_index
       && -1 != board[2].set_index
       && (pieces_set[board[1].set_index][(1 + board[1].rotation) % 4]
           != -pieces_set[board[2].set_index][(3 + board[2].rotation) % 4])){
        return false;
    }
    if(-1 != board[3].set_index
       && -1 != board[4].set_index
       && (pieces_set[board[3].set_index][(1 + board[3].rotation) % 4]
           != -pieces_set[board[4].set_index][(3 + board[4].rotation) % 4])){
        return false;
    }
    if(-1 != board[4].set_index
       && -1 != board[5].set_index
       && (pieces_set[board[4].set_index][(1 + board[4].rotation) % 4]
           != -pieces_set[board[5].set_index][(3 + board[5].rotation) % 4])){
        return false;
    }
    if(-1 != board[6].set_index
       && -1 != board[7].set_index
       && (pieces_set[board[6].set_index][(1 + board[6].rotation) % 4]
           != -pieces_set[board[7].set_index][(3 + board[7].rotation) % 4])){
        return false;
    }
    if(-1 != board[7].set_index
       && -1 != board[8].set_index
       && (pieces_set[board[7].set_index][(1 + board[7].rotation) % 4]
           != -pieces_set[board[8].set_index][(3 + board[8].rotation) % 4])){
        return false;
    }
    // Horizontal edge matching checks
    if(-1 != board[0].set_index
       && -1 != board[3].set_index
       && (pieces_set[board[0].set_index][(2 + board[0].rotation) % 4]
           != -pieces_set[board[3].set_index][(0 + board[3].rotation) % 4])){
        return false;
    }
    if(-1 != board[1].set_index
       && -1 != board[4].set_index
       && (pieces_set[board[1].set_index][(2 + board[1].rotation) % 4]
           != -pieces_set[board[4].set_index][(0 + board[4].rotation) % 4])){
        return false;
    }
    if(-1 != board[2].set_index
       && -1 != board[5].set_index
       && (pieces_set[board[2].set_index][(2 + board[2].rotation) % 4]
           != -pieces_set[board[5].set_index][(0 + board[5].rotation) % 4])){
        return false;
    }
    if(-1 != board[3].set_index
       && -1 != board[6].set_index
       && (pieces_set[board[3].set_index][(2 + board[3].rotation) % 4]
           != -pieces_set[board[6].set_index][(0 + board[6].rotation) % 4])){
        return false;
    }
    if(-1 != board[4].set_index
       && -1 != board[7].set_index
       && (pieces_set[board[4].set_index][(2 + board[4].rotation) % 4]
           != -pieces_set[board[7].set_index][(0 + board[7].rotation) % 4])){
        return false;
    }
    if(-1 != board[5].set_index
       && -1 != board[8].set_index
       && (pieces_set[board[5].set_index][(2 + board[5].rotation) % 4]
           != -pieces_set[board[8].set_index][(0 + board[8].rotation) % 4])){
        return false;
    }
    // If passed all checks, is valid board state
    return true;
}

void print_board(board_t board, bool is_solution){
    if(is_solution){
        printf("\nSOLUTION\n");
    }
    printf("%d:%d  %d:%d  %d:%d\n%d:%d  %d:%d  %d:%d\n%d:%d  %d:%d  %d:%d\n\n",
           board[0].set_index, board[0].rotation,
           board[1].set_index, board[1].rotation,
           board[2].set_index, board[2].rotation,
           board[3].set_index, board[3].rotation,
           board[4].set_index, board[4].rotation,
           board[5].set_index, board[5].rotation,
           board[6].set_index, board[6].rotation,
           board[7].set_index, board[7].rotation,
           board[8].set_index, board[8].rotation);
}
