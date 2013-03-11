require_relative './hw6provided'
require_relative './hw6assignment'

p = MyPiece.cheat_piece(MyBoard.new(MyTetris.new))
if( p.current_rotation.size ==1 && p.current_rotation[0].size == 2)   
 puts "cheat_piece pass"
else
 puts "cheat_piece fail"
end

#I'm really not sure how to test the rest. For the purpose of this assignment., my primary form
#of testing was manual testing.