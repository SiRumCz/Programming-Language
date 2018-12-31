# Programming Languages, Homework 6, hw6runner.rb

# CSC330 Fall2018 Assignment 6
# Zhe(Kevin) Chen

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:
  # class array holding all the pieces and their rotations
  # B. add three pieces
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one) (::)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T (.:.)
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two) (....)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L (..:)
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L (:..)
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S (.:`)
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z (`:.)
                    rotations([[0, 0], [0, -1], [1, 0]]), # three-squares L (:.)
                    [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], 
                    [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]], # longer I with 5 squares in two forms, center is (0, 0) (.....)
                    rotations([[0, 0], [-1, 0], [0, 1], [-1, 1], [1, 1]])] # a turtle shape? (::.) 

  My_Cheat_Piece = [[[0, 0]]] # cheat piece, single block (.)

  # Your Enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    if board.toggle_flag?
      MyPiece.new(My_Cheat_Piece, board) # toggle for cheat piece
    else
      MyPiece.new(All_My_Pieces.sample, board) # Array#sample for randomly pick
    end
  end
end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @toggle_flag = false # toggle flag for cheat, init set to false
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  # return toggle_flag
  def toggle_flag?
    @toggle_flag
  end

  # def init_label
  #   label = TetrisLabel.new(@root) do
  #     text 'Current Score: '   
  #     background 'lightblue'
  #   end
  #   label.place(35, 100, 26, 45)
  #   @score = TetrisLabel.new(@root) do
  #     background 'lightblue'
  #   end
  #   @score.text(@score)
  #   @score.place(35, 50, 126, 45)    
  # end

  # gets the next piece
  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
    @toggle_flag = false # reset flag back to false
  end

  # part A: let block to rotate 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2) # intead of call twice, change delta_rotation to 2
      # @current_block.move(0, 0, 1) # call twice to make 180 degree rotation
    end
    draw
  end

  # player acquires cheat piece
  def cheat_toggle
    if @score >= 100 and !@toggle_flag # if not sufficient points or toggle flag already on, do nothing
      @toggle_flag = true # toogle the flag for cheat piece
      @score -= 100 # consume scores
    end
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index| # was 3(fixed number), it seems to be a bug that could cause game crashes
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # small feature that allows player to accelerate block drop down
  def accelerate
    if !game_over? and @game.is_running?
      @current_block.move(0, 1, 0) # if calls '@current_block.drop_by_one', will gets werid residue blocks
    end
    draw
  end
end

class MyTetris < Tetris
  # Your Enhancements here:
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super # keeps old bindings
    # add my own bindings
    @root.bind('u', proc {@board.rotate_180}) # part A
    @root.bind('c', proc {@board.cheat_toggle}) # part C
    @root.bind('v', proc {@board.accelerate}) # extra feature: speed up the block
  end

  def buttons
    super
    # add button for speed up
    drop = TetrisButton.new('spd up', 'lightgreen'){@board.accelerate}
    drop.place(35, 50, 127, 501)
  end

  # alters the displayed score to reflect what is currently stored in the board
  # also update a GAMEOVER sign when game finished
  def update_score
    @score.text(@board.score)
    # update GAMEOVER sign when game is over
    if @board.game_over?
      endLabel = TetrisLabel.new(@root) do
        text 'GAME OVER!'
        background 'lightblue'
      end
      endLabel.place(35, 250, -23, 250)
    end
  end
end
