# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                    rotations([[0, 0], [-1, 0], [-1, 1], [0, 1], [1, 0]]), # thumbs-up
                    [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]], # longer (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]],
                    rotations([[0, 0], [0, 1], [1, 0], [0, 0]])] # heart

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0, 0], [0, 0], [0, 0], [0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat_flag = false
  end

  def run
    ran = @current_block.drop_by_one
    if !ran
      store_current
      if !game_over?
        next_piece
      end
    end
    @game.update_score
    draw
  end

  def next_piece
    if @cheat_flag then
      @current_block = MyPiece.cheat_piece(self)
      @current_pos = nil
      @cheat_flag = false
    else
      @current_block = MyPiece.next_piece(self)
      @current_pos = nil
    end
  end

  def cheat
    if @score >= 100 && !@cheat_flag then
      @score -= 100
      @cheat_flag = true
    end
  end

  # rotates the current piece clockwise
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end


  def key_bindings  
    @root.bind('n', proc {self.new_game}) 

    @root.bind('p', proc {self.pause}) 

    @root.bind('q', proc {exitProgram})
    
    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left}) 
    
    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right}) 

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise}) 

    @root.bind('u', proc {@board.rotate_180})

    @root.bind('space', proc {@board.drop_all_the_way}) 

    @root.bind('c', proc {@board.cheat})
  end
  
end


