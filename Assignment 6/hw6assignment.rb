# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],                  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]),           # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]],                      # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]),            # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]),           # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]),          # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]),          # Z
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]), # Big new piece
               [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],             # Long new piece (only needs two)
               [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
               rotations([[0, 0], [1, 0], [0, -1]])]                    # Short L new piece

  CheatPiece = [[[0, 0]]]

  # your enhancements here

  # class method to choose the next piece
  def self.next_piece (board)
    if board.isCheating
      MyPiece.new(CheatPiece, board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

  def self.numCheatBlocks
    CheatPiece[0].length
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
    @isCheating = false
  end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    sizeEnd = locations.length - 1

    (0..sizeEnd).each{|index|       # change loop # so it can vary depending on piece size
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # moves the current piece down by one, if this is not possible stores the
  # current piece and replaces it with a new one.
  def run
    ran = @current_block.drop_by_one
    if !ran
      store_current
      if !game_over?
        if @current_block.current_rotation.length == MyPiece.numCheatBlocks
          @isCheating = false     # clears the users name of cheating
        end
          next_piece
      end
    end
    @game.update_score
    draw
  end

  # drops the piece to the lowest location in the currently occupied columns.
  # Then replaces it with a new piece
  # Change the score to reflect the distance dropped.
  def drop_all_the_way
    if @game.is_running?
      ran = @current_block.drop_by_one
      @current_pos.each{|block| block.remove}
      while ran
        @score += 1
        ran = @current_block.drop_by_one
      end
      draw
      store_current
      if !game_over?
        if @current_block.current_rotation.length == MyPiece.numCheatBlocks
          @isCheating = false     # clears the users name of cheating
        end
          next_piece
      end
      @game.update_score
      draw
    end
  end

  def isCheating
    @isCheating
  end

  def cheat
    if @score >= 100 and !@isCheating
      puts 'Cheat piece avaliable next turn'
      @isCheating = true
      @score -= 100
    elsif @isCheating
      puts 'You\'re already cheating'
    else
      puts 'You need a score of at least 100 to cheat'
    end
  end
end

class MyTetris < Tetris
  # your enhancements here

  # creates a canvas and the board that interacts with it
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
    
    @root.bind('u', proc {2.times {@board.rotate_clockwise}})
    
    @root.bind('space', proc {@board.drop_all_the_way}) 

    @root.bind('c', proc {@board.cheat})
  end
end
