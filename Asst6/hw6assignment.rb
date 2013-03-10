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
                 
end

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

end

class MyBoard < Board


end
