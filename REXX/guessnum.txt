/* REXX */
say "I'm thinking of a number between 1 and 10."
secret = RANDOM(1,10)
tries = 1

do while (guess \= secret)
    say "What is your guess?"
    pull guess
    if (datatype(guess, 'M')) then
        say "Bozo NUMBERS please"
    else
    if (datatype(guess, 'N')) then
        if (guess < 1 ) then
            say "Id10t BETWEEN 1 and 10"
        else
            if (guess > 10) then
                say "Id10t BETWEEN 1 and 10"
            else
                if (guess \= secret) then
                do
                    say "That's not it. Try again"
                    if (guess < secret) then
                        say "to low"
                    else
                        say "to high"
                    tries = tries + 1
    end
end
say "You got it! And it only took you" tries "tries!"
exit
