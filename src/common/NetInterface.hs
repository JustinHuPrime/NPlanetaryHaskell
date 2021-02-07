module NetInterface where

port :: [Char]
port = "3000"

-- on connection, server waits for client to say hello (expects some particular handshake string)
-- server replies with current game state
-- server waits until client sends a move
-- if everyone has sent in a move, this thread handles the list of moves
-- otherwise, this thread adds the move to the list of moves