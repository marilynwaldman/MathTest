# MathTest

input: question bank:  /data/questions2
output;  Two tables:

1.  dfgrades - a gradebook - list of questons with the answers and responses
2.  dfanswers - a listing of responses from student in the order that they were answered.

Algorithm:

for each question, present the set of choices.  If "submit" is pressed then the answer is recorded in the gradebook and the answer list.

Student can navigate throught the list of questions with the "next" and "previous" button.

Termination is yet to be implemented. the "finish" button doesn't work yet

Currently if student enters three successive wrong answers, the program prints "finished" in the terminal. Behavior for this condition is yet to be decided.

Entire stack will be refactored.


