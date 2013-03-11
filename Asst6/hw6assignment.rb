# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyTetris < Tetris
  #enh 1, 'u' rotates 180degrees
  def key_bindings
    super
    @root.bind('u', 
               proc {
                 @board.rotate_counter_clockwise;
                 @board.rotate_counter_clockwise;
               })
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
                 
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces +
    [rotations([[0,0],[-1,0],[-1,1],[0,1],[1,0]]), #New piece 1
    [[[0,0],[-1,0],[-2,0],[1,0],[2,0]], #New piece 2
     [[0,0],[0,-1],[0,-2],[0,1],[0,2]]],
    rotations([[0,0],[-1,0],[-1,1]])] #New piece 3

  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

end

class MyBoard < Board
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end
