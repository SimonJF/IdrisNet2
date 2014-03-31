module GameMessage

-- Data structure communicated between aux thread and master thread,
-- signifying some update.

data GameMessage = UpdateRemotePaddle Int Int
                 -- My plan is currently to send a UDP packet every time the
                 -- ball updates. Not hugely efficient: it would be far better
                 -- to just encode movement vectors, but let's see how it goes
                 | UpdateRemoteBallPos Int Int 
