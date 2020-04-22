/**
 * \file puzzle.c
 *
 * \brief Solves a scramble-squares puzzle
 *
 * \copyright Copyright (c) 2020, Kevin Kredit.
 */


/******************************************************************************
 *                                                                 Inclusions */
#include <stdio.h>
#include <stdbool.h>
#include <string.h>


/******************************************************************************
 *                                                                      Types */
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

/* Piece
 *      edge 1
 *    e       e
 *    d       d
 *    g       g
 *    e       e
 *    4       2
 *      edge 3
 */
typedef edge_t piece_t[4];


typedef struct placed_piece_s {
    int         set_index;
    int         rotation;
} placed_piece_t;

/* Board
 *    1 2 3
 *    4 5 6
 *    7 8 9
 */
typedef placed_piece_t board_t[9];


/******************************************************************************
 *                                                      Function declarations */
bool board_is_legal(board_t board, int index);
void print_board(board_t board);
void recursive_check(board_t board, int current_index);


/******************************************************************************
 *                                                                       Data */
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

unsigned long num_boards_checked = 0;


/******************************************************************************
 *                                                         External functions */
int main(void) {

    board_t board = {
        {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0}, {-1, 0},
    };

    printf("Working on the solution to the puzzle...\n");
    recursive_check(board, 0);
    printf("Checked %lu boards\n", num_boards_checked);

    return 0;
}


/******************************************************************************
 *                                                         Internal functions */
void recursive_check(board_t board, int current_index){
    // Determine which pieces are still available
    bool unplaced_indices[9] = {true, true, true, true, true, true, true, true, true};
    for(int i = 0; i < 9; i++){
        if(-1 != board[i].set_index){
            unplaced_indices[board[i].set_index] = false;
        }
    }

    // For each available piece
    for(int index = 0; index < 9; index++){
        // If unplaced
        if(unplaced_indices[index]){
            // For each rotation
            for(int rot_pos = 0; rot_pos < 4; rot_pos++){
                // Add to board, check if legal, follow logic outlined above
                board[current_index].set_index = index;
                board[current_index].rotation  = rot_pos;
                if(board_is_legal(board, current_index)){
                    if(8 == current_index){
                        print_board(board);
                    }
                    else {
                        board_t board_copy;
                        memcpy(board_copy, board, sizeof(board_t));
                        recursive_check(board_copy, current_index + 1);
                    }
                }
            }
        }
    }
}

// Only check latest piece
bool board_is_legal(board_t board, int index){
    num_boards_checked++;
    bool legal = true;

    bool top_row = index < 3;
    bool left_col = 0 == index % 3;
    const piece_t *this_p = &pieces_set[board[index].set_index];
    const int this_r = board[index].rotation;
    if(!top_row) {
        const piece_t *above_p = &pieces_set[board[index - 3].set_index];
        const int above_r = board[index - 3].rotation;
        legal &= ((*above_p)[(2 + above_r) % 4] == -(*this_p)[(0 + this_r) % 4]);
    }
    if(!left_col) {
        const piece_t *left_p = &pieces_set[board[index - 1].set_index];
        const int left_r = board[index - 1].rotation;
        legal &= ((*left_p)[(1 + left_r) % 4] == -(*this_p)[(3 + this_r) % 4]);
    }
    return legal;
}

void print_board(board_t board){
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
