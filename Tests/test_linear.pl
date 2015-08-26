%% %% TEST 1
%% 'x(0)'(A) :- A>0.
%% 'x(0)'(A) :- 'x(0)'(A).
%% 'x(2)'(A) :- 'x(1)'(A),'x(1)'(A).
%% 'x(1)'(A) :- 'x(0)'(A), 'x(0)'(A).
%% 'x(3)'(A) :- 'x(2)'(A), 'x(2)'(A).
%% 'x(4)'(A) :- 'x(3)'(A), 'x(3)'(A).
%% 'x(5)'(A) :- 'x(4)'(A), 'x(4)'(A).

%OUTPUT

%% Eureka Definitions:
%% 'new31(5)'(A) :- 'new15(4)'(A), 'x(4)'(A).
%% 'new30(5)'(A) :- 'new14(4)'(A), 'x(4)'(A).
%% 'new29(5)'(A) :- 'new13(4)'(A), 'x(4)'(A).
%% 'new28(5)'(A) :- 'new12(4)'(A), 'x(4)'(A).
%% 'new27(5)'(A) :- 'new11(4)'(A), 'x(4)'(A).
%% 'new26(5)'(A) :- 'new10(4)'(A), 'x(4)'(A).
%% 'new25(5)'(A) :- 'new9(4)'(A), 'x(4)'(A).
%% 'new24(5)'(A) :- 'new8(4)'(A), 'x(4)'(A).
%% 'new23(5)'(A) :- 'new7(3)'(A), 'x(4)'(A).
%% 'new22(5)'(A) :- 'new6(3)'(A), 'x(4)'(A).
%% 'new21(5)'(A) :- 'new5(3)'(A), 'x(4)'(A).
%% 'new20(5)'(A) :- 'new4(3)'(A), 'x(4)'(A).
%% 'new19(5)'(A) :- 'new3(2)'(A), 'x(4)'(A).
%% 'new18(5)'(A) :- 'new2(2)'(A), 'x(4)'(A).
%% 'new17(5)'(A) :- 'new1(1)'(A), 'x(4)'(A).
%% 'new16(5)'(A) :- 'x(0)'(A), 'x(4)'(A).
%% 'new15(4)'(A) :- 'new7(3)'(A), 'x(3)'(A).
%% 'new14(4)'(A) :- 'new6(3)'(A), 'x(3)'(A).
%% 'new13(4)'(A) :- 'new5(3)'(A), 'x(3)'(A).
%% 'new12(4)'(A) :- 'new4(3)'(A), 'x(3)'(A).
%% 'new11(4)'(A) :- 'new3(2)'(A), 'x(3)'(A).
%% 'new10(4)'(A) :- 'new2(2)'(A), 'x(3)'(A).
%% 'new9(4)'(A) :- 'new1(1)'(A), 'x(3)'(A).
%% 'new8(4)'(A) :- 'x(0)'(A), 'x(3)'(A).
%% 'new7(3)'(A) :- 'new3(2)'(A), 'x(2)'(A).
%% 'new6(3)'(A) :- 'new2(2)'(A), 'x(2)'(A).
%% 'new5(3)'(A) :- 'new1(1)'(A), 'x(2)'(A).
%% 'new4(3)'(A) :- 'x(0)'(A), 'x(2)'(A).
%% 'new3(2)'(A) :- 'new1(1)'(A), 'x(1)'(A).
%% 'new2(2)'(A) :- 'x(0)'(A), 'x(1)'(A).
%% 'new1(1)'(A) :- 'x(0)'(A), 'x(0)'(A).
%% Linearised Program:
%% 'x(0)'(A) :- A>0.
%% 'x(0)'(A) :- 'x(0)'(A).
%% 'x(1)'(A) :- A>0, 'x(0)'(A).
%% 'x(1)'(A) :- 'new1(1)'(A).
%% 'new1(1)'(A) :- A>0, 'x(0)'(A).
%% 'new1(1)'(A) :- 'new1(1)'(A).
%% 'x(2)'(A) :- A>0, 'x(1)'(A).
%% 'x(2)'(A) :- A>0, 'new2(2)'(A).
%% 'x(2)'(A) :- 'new3(2)'(A).
%% 'new2(2)'(A) :- A>0, 'x(1)'(A).
%% 'new2(2)'(A) :- 'new2(2)'(A).
%% 'new3(2)'(A) :- A>0, 'new2(2)'(A).
%% 'new3(2)'(A) :- 'new3(2)'(A).
%% 'x(3)'(A) :- A>0, 'x(2)'(A).
%% 'x(3)'(A) :- A>0, 'new4(3)'(A).
%% 'x(3)'(A) :- A>0, 'new5(3)'(A).
%% 'x(3)'(A) :- A>0, 'new6(3)'(A).
%% 'x(3)'(A) :- 'new7(3)'(A).
%% 'new4(3)'(A) :- A>0, 'x(2)'(A).
%% 'new4(3)'(A) :- 'new4(3)'(A).
%% 'new5(3)'(A) :- A>0, 'new4(3)'(A).
%% 'new5(3)'(A) :- 'new5(3)'(A).
%% 'new6(3)'(A) :- 'new6(3)'(A).
%% 'new6(3)'(A) :- A>0, 'new4(3)'(A).
%% 'new6(3)'(A) :- A>0, 'new5(3)'(A).
%% 'new7(3)'(A) :- A>0, 'new6(3)'(A).
%% 'new7(3)'(A) :- 'new7(3)'(A).
%% 'x(4)'(A) :- A>0, 'x(3)'(A).
%% 'x(4)'(A) :- A>0, 'new8(4)'(A).
%% 'x(4)'(A) :- A>0, 'new9(4)'(A).
%% 'x(4)'(A) :- A>0, 'new10(4)'(A).
%% 'x(4)'(A) :- A>0, 'new11(4)'(A).
%% 'x(4)'(A) :- A>0, 'new12(4)'(A).
%% 'x(4)'(A) :- A>0, 'new13(4)'(A).
%% 'x(4)'(A) :- A>0, 'new14(4)'(A).
%% 'x(4)'(A) :- 'new15(4)'(A).
%% 'new8(4)'(A) :- A>0, 'x(3)'(A).
%% 'new8(4)'(A) :- 'new8(4)'(A).
%% 'new9(4)'(A) :- A>0, 'new8(4)'(A).
%% 'new9(4)'(A) :- 'new9(4)'(A).
%% 'new10(4)'(A) :- 'new10(4)'(A).
%% 'new10(4)'(A) :- A>0, 'new8(4)'(A).
%% 'new10(4)'(A) :- A>0, 'new9(4)'(A).
%% 'new11(4)'(A) :- A>0, 'new10(4)'(A).
%% 'new11(4)'(A) :- 'new11(4)'(A).
%% 'new12(4)'(A) :- 'new12(4)'(A).
%% 'new12(4)'(A) :- A>0, 'new10(4)'(A).
%% 'new12(4)'(A) :- A>0, 'new11(4)'(A).
%% 'new12(4)'(A) :- A>0, 'new8(4)'(A).
%% 'new12(4)'(A) :- A>0, 'new9(4)'(A).
%% 'new13(4)'(A) :- A>0, 'new12(4)'(A).
%% 'new13(4)'(A) :- 'new13(4)'(A).
%% 'new14(4)'(A) :- 'new14(4)'(A).
%% 'new14(4)'(A) :- A>0, 'new12(4)'(A).
%% 'new14(4)'(A) :- A>0, 'new13(4)'(A).
%% 'new15(4)'(A) :- A>0, 'new14(4)'(A).
%% 'new15(4)'(A) :- 'new15(4)'(A).
%% 'x(5)'(A) :- A>0, 'x(4)'(A).
%% 'x(5)'(A) :- A>0, 'new16(5)'(A).
%% 'x(5)'(A) :- A>0, 'new17(5)'(A).
%% 'x(5)'(A) :- A>0, 'new18(5)'(A).
%% 'x(5)'(A) :- A>0, 'new19(5)'(A).
%% 'x(5)'(A) :- A>0, 'new20(5)'(A).
%% 'x(5)'(A) :- A>0, 'new21(5)'(A).
%% 'x(5)'(A) :- A>0, 'new22(5)'(A).
%% 'x(5)'(A) :- A>0, 'new23(5)'(A).
%% 'x(5)'(A) :- A>0, 'new24(5)'(A).
%% 'x(5)'(A) :- A>0, 'new25(5)'(A).
%% 'x(5)'(A) :- A>0, 'new26(5)'(A).
%% 'x(5)'(A) :- A>0, 'new27(5)'(A).
%% 'x(5)'(A) :- A>0, 'new28(5)'(A).
%% 'x(5)'(A) :- A>0, 'new29(5)'(A).
%% 'x(5)'(A) :- A>0, 'new30(5)'(A).
%% 'x(5)'(A) :- 'new31(5)'(A).
%% 'new16(5)'(A) :- A>0, 'x(4)'(A).
%% 'new16(5)'(A) :- 'new16(5)'(A).
%% 'new17(5)'(A) :- A>0, 'new16(5)'(A).
%% 'new17(5)'(A) :- 'new17(5)'(A).
%% 'new18(5)'(A) :- 'new18(5)'(A).
%% 'new18(5)'(A) :- A>0, 'new16(5)'(A).
%% 'new18(5)'(A) :- A>0, 'new17(5)'(A).
%% 'new19(5)'(A) :- A>0, 'new18(5)'(A).
%% 'new19(5)'(A) :- 'new19(5)'(A).
%% 'new20(5)'(A) :- 'new20(5)'(A).
%% 'new20(5)'(A) :- A>0, 'new18(5)'(A).
%% 'new20(5)'(A) :- A>0, 'new19(5)'(A).
%% 'new20(5)'(A) :- A>0, 'new16(5)'(A).
%% 'new20(5)'(A) :- A>0, 'new17(5)'(A).
%% 'new21(5)'(A) :- A>0, 'new20(5)'(A).
%% 'new21(5)'(A) :- 'new21(5)'(A).
%% 'new22(5)'(A) :- 'new22(5)'(A).
%% 'new22(5)'(A) :- A>0, 'new20(5)'(A).
%% 'new22(5)'(A) :- A>0, 'new21(5)'(A).
%% 'new23(5)'(A) :- A>0, 'new22(5)'(A).
%% 'new23(5)'(A) :- 'new23(5)'(A).
%% 'new24(5)'(A) :- 'new24(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new20(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new21(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new22(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new23(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new18(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new19(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new16(5)'(A).
%% 'new24(5)'(A) :- A>0, 'new17(5)'(A).
%% 'new25(5)'(A) :- A>0, 'new24(5)'(A).
%% 'new25(5)'(A) :- 'new25(5)'(A).
%% 'new26(5)'(A) :- 'new26(5)'(A).
%% 'new26(5)'(A) :- A>0, 'new24(5)'(A).
%% 'new26(5)'(A) :- A>0, 'new25(5)'(A).
%% 'new27(5)'(A) :- A>0, 'new26(5)'(A).
%% 'new27(5)'(A) :- 'new27(5)'(A).
%% 'new28(5)'(A) :- 'new28(5)'(A).
%% 'new28(5)'(A) :- A>0, 'new26(5)'(A).
%% 'new28(5)'(A) :- A>0, 'new27(5)'(A).
%% 'new28(5)'(A) :- A>0, 'new24(5)'(A).
%% 'new28(5)'(A) :- A>0, 'new25(5)'(A).
%% 'new29(5)'(A) :- A>0, 'new28(5)'(A).
%% 'new29(5)'(A) :- 'new29(5)'(A).
%% 'new30(5)'(A) :- 'new30(5)'(A).
%% 'new30(5)'(A) :- A>0, 'new28(5)'(A).
%% 'new30(5)'(A) :- A>0, 'new29(5)'(A).
%% 'new31(5)'(A) :- A>0, 'new30(5)'(A).
%% 'new31(5)'(A) :- 'new31(5)'(A).

%% TEST 2
%% 'x(1)'(A,B) :- E>0, 'y(0)'(A,C), 'z(0)'(C,E), 'y[0]'(E,B).
%% 'y(0)'(A,B) :- 'y(0)'(A,B).
%% 'y(0)'(A,B) :- A>0, B>0.
%% 'z(0)'(A,B) :- 'z(0)'(A,B).
%% 'z(0)'(A,B) :- A>0,B>0.

%% SOLUTION
%% Eureka Definitions:
%% 'new2(1)'(A,B,C) :- 'z(0)'(C,B), 'y[0]'(B,A).
%% 'new1(1)'(A,B,C) :- 'y(0)'(A,D), 'z(0)'(D,C), 'y[0]'(C,B).
%% Linearised Program:
%% 'y(0)'(A,B) :- 'y(0)'(A,B).
%% 'y(0)'(A,B) :- A>0, B>0.
%% 'z(0)'(A,B) :- 'z(0)'(A,B).
%% 'z(0)'(A,B) :- A>0, B>0.
%% 'x(1)'(A,B) :- C>0, A>0, D>0, 'y[0]'(C,B).
%% 'x(1)'(A,B) :- C>0, 'new1(1)'(A,B,C).
%% 'x(1)'(A,B) :- C>0, A>0, D>0, 'new2(1)'(B,C,D).
%% 'new1(1)'(A,B,C) :- 'new1(1)'(A,B,C).
%% 'new1(1)'(A,B,C) :- A>0, D>0, 'new2(1)'(B,C,D).
%% 'new2(1)'(A,B,C) :- C>0, B>0, 'y[0]'(B,A).
%% 'new2(1)'(A,B,C) :- 'new2(1)'(A,B,C).

%% TEST 3
%% 'x(2)'(A,B):-'x(2)'(A,B),'x[1]'(A,B).
%% 'x(2)'(A,B):-'x(1)'(A,B),'x(1)'(A,B).
%% 'x(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).
%% 'x(1)'(A,B):-'d(1)'(A,C),'x[0]'(C,B),A>0.
%% 'd(1)'(A,B):-'f(1)'(A,B).
%% 'f(1)'(A,B):-'f(1)'(A,B).
%% 'x(0)'(A,B):-A>B.
%% 'x(0)'(A,B):-A>C,'x(0)'(C,B).
%% 'x[1]'(A,B):-'x(1)'(A,B).
%% 'x[1]'(A,B):-'x(0)'(A,B).
%% 'x[0]'(A,B):-'x(0)'(A,B).

%% %% SOLUTION
%% Eureka Definitions:
%% 'new10(2)'(A,B,C,D) :- 'new2(1)'(A,B,C,D), 'x(1)'(A,B).
%% 'new9(2)'(A,B,C) :- 'f(1)'(A,C), 'x(1)'(A,B).
%% 'new8(2)'(A,B,C) :- 'new1(1)'(B,C), 'x(1)'(A,B).
%% 'new7(2)'(A,B,C) :- 'x(0)'(C,B), 'x(1)'(A,B).
%% 'new6(2)'(A,B,C,D) :- 'x(2)'(A,B), 'new2(1)'(A,B,C,D).
%% 'new5(2)'(A,B,C) :- 'x(2)'(A,B), 'f(1)'(A,C).
%% 'new4(2)'(A,B,C) :- 'x(2)'(A,B), 'new1(1)'(B,C).
%% 'new3(2)'(A,B,C) :- 'x(2)'(A,B), 'x(0)'(C,B).
%% 'new2(1)'(A,B,C,D) :- 'd(1)'(A,C), 'x(0)'(D,B).
%% 'new1(1)'(A,B) :- 'x(0)'(B,C), 'x(0)'(C,A).
%% Linearised Program:
%% 'd(1)'(A,B) :- 'f(1)'(A,B).
%% 'f(1)'(A,B) :- 'f(1)'(A,B).
%% 'x(0)'(A,B) :- A>B.
%% 'x(0)'(A,B) :- A>C, 'x(0)'(C,B).
%% 'x[1]'(A,B) :- 'x(1)'(A,B).
%% 'x[1]'(A,B) :- 'x(0)'(A,B).
%% 'x[0]'(A,B) :- 'x(0)'(A,B).
%% 'x(1)'(A,B) :- A>C, 'x(0)'(C,B).
%% 'x(1)'(A,B) :- A>C, 'new1(1)'(B,C).
%% 'new1(1)'(A,B) :- B>C, 'x(0)'(C,A).
%% 'new1(1)'(A,B) :- B>C, 'new1(1)'(A,C).
%% 'x(1)'(A,B) :- A>0, C>B, 'd(1)'(A,C).
%% 'x(1)'(A,B) :- A>0, C>D, D>B, 'd(1)'(A,C).
%% 'x(1)'(A,B) :- A>0, C>D, D>E, 'new2(1)'(A,B,C,E).
%% 'new2(1)'(A,B,C,D) :- D>B, 'd(1)'(A,C).
%% 'new2(1)'(A,B,C,D) :- D>E, 'new2(1)'(A,B,C,E).
%% 'x(2)'(A,B) :- A>C, C>B, 'x(2)'(A,B).
%% 'x(2)'(A,B) :- A>B, 'x(2)'(A,B).
%% 'x(2)'(A,B) :- A>C, C>D, 'new3(2)'(A,B,D).
%% 'x(2)'(A,B) :- A>C, C>D, 'new4(2)'(A,B,D).
%% 'x(2)'(A,B) :- A>0, C>B, 'new5(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>0, C>D, D>B, 'new5(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>0, C>D, D>E, E>F, 'new6(2)'(A,B,C,F).
%% 'x(2)'(A,B) :- A>0, C>D, D>E, E>B, 'new5(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>C, 'new3(2)'(A,B,C).
%% 'new3(2)'(A,B,C) :- C>B, 'x(2)'(A,B).
%% 'new3(2)'(A,B,C) :- C>D, 'new3(2)'(A,B,D).
%% 'new4(2)'(A,B,C) :- C>D, 'new3(2)'(A,B,D).
%% 'new4(2)'(A,B,C) :- C>D, 'new4(2)'(A,B,D).
%% 'new5(2)'(A,B,C) :- 'new5(2)'(A,B,C).
%% 'new6(2)'(A,B,C,D) :- D>E, 'new6(2)'(A,B,C,E).
%% 'new6(2)'(A,B,C,D) :- D>B, 'new5(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>C, C>B, 'x(1)'(A,B).
%% 'x(2)'(A,B) :- A>C, C>D, 'new7(2)'(A,B,D).
%% 'x(2)'(A,B) :- A>C, C>D, 'new8(2)'(A,B,D).
%% 'x(2)'(A,B) :- A>0, C>B, 'new9(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>0, C>D, D>B, 'new9(2)'(A,B,C).
%% 'x(2)'(A,B) :- A>0, C>D, D>E, E>F, 'new10(2)'(A,B,C,F).
%% 'x(2)'(A,B) :- A>0, C>D, D>E, E>B, 'new9(2)'(A,B,C).
%% 'new7(2)'(A,B,C) :- C>B, 'x(1)'(A,B).
%% 'new7(2)'(A,B,C) :- C>D, 'new7(2)'(A,B,D).
%% 'new8(2)'(A,B,C) :- C>D, 'new7(2)'(A,B,D).
%% 'new8(2)'(A,B,C) :- C>D, 'new8(2)'(A,B,D).
%% 'new9(2)'(A,B,C) :- 'new9(2)'(A,B,C).
%% 'new10(2)'(A,B,C,D) :- D>E, 'new10(2)'(A,B,C,E).
%% 'new10(2)'(A,B,C,D) :- D>B, 'new9(2)'(A,B,C).

%% TEST 4
%% 'w(1)'(A,B):-'x[0]'(A,C),'x(0)'(C,B).
%% 'y(1)'(A,B):-'x(0)'(A,B),'x(0)'(B,A).
%% 'x(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).
%% 'x(0)'(C,D):-C>0,'x(0)'(C,D).
%% 'x[0]'(A,B):-'x(0)'(A,B).

%% SOLUTION
%% Eureka Definitions:
%% 'new2(1)'(A,B) :- 'x(0)'(A,B), 'x(0)'(B,A).
%% 'new1(1)'(A,B) :- 'x(0)'(A,C), 'x(0)'(C,B).
%% Linearised Program:
%% 'x(0)'(A,B) :- A>0, 'x(0)'(A,B).
%% 'x[0]'(A,B) :- 'x(0)'(A,B).
%% 'w(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% 'new1(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% 'y(1)'(A,B) :- A>0, 'new2(1)'(A,B).
%% 'new2(1)'(A,B) :- A>0, 'new2(1)'(A,B).
%% 'x(1)'(A,B) :- A>0, 'new1(1)'(A,B).

%% TEST 5
%% 'x(1)'(A,B,C):-'x[0]'(A,Y,Z),'x(0)'(Y,B,Z),'x(0)'(Y,Z,C).
%% 'x(1)'(A,B,C):-'x[0]'(A,Y,Z),'x(0)'(B,Y,Z),'x(0)'(C,Y,Z).
%% 'x(1)'(A,B,C):-'x[0]'(Y,A,Z),'x(0)'(Y,B,Z),'x(0)'(Y,C,Z).
%% 'x(0)'(A,B,C):-C>0,'x(0)'(A,B,C).
%% 'x[0]'(A,B,C):-'x(0)'(A,B,C).

%%SOLUTION

%% Eureka Definitions:
%% 'new3(1)'(A,B,C,D) :- 'x(0)'(E,A,D), 'x(0)'(E,B,D), 'x(0)'(E,C,D).
%% 'new2(1)'(A,B,C,D) :- 'x(0)'(A,E,D), 'x(0)'(B,E,D), 'x(0)'(C,E,D).
%% 'new1(1)'(A,B,C,D) :- 'x(0)'(A,E,D), 'x(0)'(E,B,D), 'x(0)'(E,D,C).
%% Linearised Program:
%% 'x(0)'(A,B,C) :- C>0, 'x(0)'(A,B,C).
%% 'x[0]'(A,B,C) :- 'x(0)'(A,B,C).
%% 'x(1)'(A,B,C) :- D>0, 'new1(1)'(A,B,C,D).
%% 'new1(1)'(A,B,C,D) :- D>0, 'new1(1)'(A,B,C,D).
%% 'x(1)'(A,B,C) :- D>0, 'new2(1)'(A,B,C,D).
%% 'new2(1)'(A,B,C,D) :- D>0, 'new2(1)'(A,B,C,D).
%% 'x(1)'(A,B,C) :- D>0, 'new3(1)'(A,B,C,D).
%% 'new3(1)'(A,B,C,D) :- D>0, 'new3(1)'(A,B,C,D).

%%TEST 6
%% 'x(1)'(A,B,C):-'x[0]'(A,Y,Z),'x(0)'(Y,B,Z),'x(0)'(Y,Z,C).
%% 'x(1)'(A,B,C):-'x[0]'(A,Y,Z),'x(0)'(B,Y,Z),'x(0)'(C,Y,Z).
%% 'x(1)'(A,B,C):-'x[0]'(A,Z,Y),'x(0)'(Z,B,Y),'x(0)'(Z,Y,C).
%% 'x(0)'(A,B,C):-C>0,'x(0)'(A,B,C).
%% 'x[0]'(A,B,C):-'x(0)'(A,B,C).

%%SOLUTION

%% Eureka Definitions:
%% 'new2(1)'(A,B,C,D) :- 'x(0)'(A,E,D), 'x(0)'(B,E,D), 'x(0)'(C,E,D).
%% 'new1(1)'(A,B,C,D) :- 'x(0)'(A,E,D), 'x(0)'(E,B,D), 'x(0)'(E,D,C).
%% Linearised Program:
%% 'x(0)'(A,B,C) :- C>0, 'x(0)'(A,B,C).
%% 'x[0]'(A,B,C) :- 'x(0)'(A,B,C).
%% 'x(1)'(A,B,C) :- D>0, 'new1(1)'(A,B,C,D).
%% 'new1(1)'(A,B,C,D) :- D>0, 'new1(1)'(A,B,C,D).
%% 'x(1)'(A,B,C) :- D>0, 'new2(1)'(A,B,C,D).
%% 'new2(1)'(A,B,C,D) :- D>0, 'new2(1)'(A,B,C,D).
%% 'x(1)'(A,B,C) :- 'new1(1)'(A,B,C,D).

%% TEST 9 [Useless clauses do not affect to linearisation procedure. 
%% Considering y(1) as the axiom of the program, x(1) and x(2) are useless predicates]
%% 'x(0)'(A,B):-A>0,'x(0)'(A,B).
%% 'y(0)'(A,B):-B>0,'y(0)'(A,B).
%% 'y(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).

%% U'x(1)'(A,B):-'y(0)'(A,B),'x(0)'(A,B).
%% U'x(2)'(A,B):-'x(1)'(A,B),'x(1)'(A,B).
%% U'x(2)'(A,B):-'y(1)'(A,B),'y(1)'(A,B).

%% SOLUTION

%% Eureka Definitions:
%% U 'new4(2)'(A,B) :- 'new1(1)'(A,B), 'y(1)'(A,B).
%% U 'new3(2)'(A,B) :- 'new2(1)'(A,B), 'x(1)'(A,B).
%% U 'new2(1)'(A,B) :- 'y(0)'(A,B), 'x(0)'(A,B).
%% 'new1(1)'(A,B) :- 'x(0)'(A,C), 'x(0)'(C,B).
%% Linearised Program:
%% 'x(0)'(A,B) :- A>0, 'x(0)'(A,B).
%% 'y(0)'(A,B) :- B>0, 'y(0)'(A,B).
%% 'y(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% 'new1(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% U 'x(1)'(A,B) :- B>0, 'new2(1)'(A,B).
%% U 'new2(1)'(A,B) :- B>0, 'new2(1)'(A,B).
%% U 'x(2)'(A,B) :- B>0, 'new3(2)'(A,B).
%% U 'new3(2)'(A,B) :- B>0, 'new3(2)'(A,B).
%% U 'x(2)'(A,B) :- A>0, 'new4(2)'(A,B).
%% U 'new4(2)'(A,B) :- A>0, 'new4(2)'(A,B).

%% PROGRAM AFTER REMOVING USELESS CLAUSES (U)
%% 'x(0)'(A,B):-A>0,'x(0)'(A,B).
%% 'y(0)'(A,B):-B>0,'y(0)'(A,B).
%% 'y(1)'(A,B):-'x(0)'(A,C),'x(0)'(C,B).

%% SOLUTION [Removing useless clauses (U) from the previous program does not affect the ouput of the procedure]

%% Eureka Definitions:
%% 'new1(1)'(A,B) :- 'x(0)'(A,C), 'x(0)'(C,B).
%% Linearised Program:
%% 'x(0)'(A,B) :- A>0, 'x(0)'(A,B).
%% 'y(0)'(A,B) :- B>0, 'y(0)'(A,B).
%% 'y(1)'(A,B) :- A>0, 'new1(1)'(A,B).
%% 'new1(1)'(A,B) :- A>0, 'new1(1)'(A,B).


%%TEST 10 

%% If the set of variables appearing in the predicates of the eurekable body do not
%% occur in the rest of the clause, the eureka predicate is defined on all those variables.
%% Other wise the ones we do not include would depend on those we include.

%% 'p(1)'(A):-A>0,'d(0)'(B,C),'d(0)'(C,B).
%% 'd(0)'(A,B):-'d(0)'(A,B).

%% Eureka Definitions:
%% 'new1(1)'(A,B) :- 'd(0)'(A,B), 'd(0)'(B,A).
%% Linearised Program:
%% 'd(0)'(A,B) :- 'd(0)'(A,B).
%% 'p(1)'(A) :- A>0, 'new1(1)'(B,C).
%% 'new1(1)'(A,B) :- 'new1(1)'(A,B).

%%TEST 11 

%% ELP selects in turns minimally non-linar clauses, starting with those of index k and
%% once every non-linear clause  of index k is linearised, continuing with those of index k+1.

%% 'p(1)'(A,B):-'y(1)'(A,B),'y[0]'(A,B).
%% 'd(1)'(A,B):-'p(1)'(A,B),'y[0]'(A,B).
%% 'z(2)'(A,B):- 'y(1)'(A,C),'y(1)'(C,B).
%% 'y(1)'(A,B):- 'x(1)'(A,C),'x(0)'(C,B).
%% 'x(1)'(A,B):- 'y(1)'(A,B),'x(0)'(A,B).
%% 'x(1)'(A,B):- 'x(0)'(A,C),'x(0)'(C,B).
%% 'x(0)'(A,B):-A>0,'x(0)'(A,B).
%% 'x(0)'(A,B):-A>0,B>0.
%% 'y[0]'(A,B):-'y(0)'(A,B).
%% 'd(1)'(A,B):-'d(1)'(A,B).
%% 'p(1)'(A,B):-'p(1)'(A,B).

%% Eureka Definitions:
%% 'new9(2)'(A,B,C) :- 'new1(1)'(A,D,C), 'y(1)'(D,B).
%% 'new8(2)'(A,B,C,D) :- 'new3(1)'(A,C), 'y(1)'(D,B).
%% 'new7(2)'(A,B,C,D) :- 'x(0)'(D,B), 'y(1)'(C,A).
%% 'new6(2)'(A,B,C,D) :- 'new2(1)'(A,C), 'y(1)'(D,B).
%% 'new5(2)'(A,B,C,D,E) :- 'new1(1)'(A,C,E), 'y(1)'(D,B).
%% 'new4(2)'(A,B,C,D) :- 'x(1)'(A,D), 'y(1)'(C,B).
%% 'new3(1)'(A,B) :- 'x(0)'(A,C), 'x(0)'(C,B).
%% 'new2(1)'(A,B) :- 'y(1)'(A,B), 'x(0)'(A,B).
%% 'new1(1)'(A,B,C) :- 'x(1)'(A,C), 'x(0)'(C,B).
%% Linearised Program:
%% 'x(0)'(A,B) :- A>0, 'x(0)'(A,B).
%% 'x(0)'(A,B) :- A>0, B>0.
%% 'y[0]'(A,B) :- 'y(0)'(A,B).
%% 'd(1)'(A,B) :- 'd(1)'(A,B).
%% 'p(1)'(A,B) :- 'p(1)'(A,B).
%% 'y(1)'(A,B) :- C>0, B>0, 'x(1)'(A,C).
%% 'y(1)'(A,B) :- C>0, 'new1(1)'(A,B,C).
%% 'new1(1)'(A,B,C) :- C>0, B>0, 'x(1)'(A,C).
%% 'new1(1)'(A,B,C) :- C>0, 'new1(1)'(A,B,C).
%% 'x(1)'(A,B) :- A>0, B>0, 'y(1)'(A,B).
%% 'x(1)'(A,B) :- A>0, 'new2(1)'(A,B).
%% 'new2(1)'(A,B) :- A>0, B>0, 'y(1)'(A,B).
%% 'new2(1)'(A,B) :- A>0, 'new2(1)'(A,B).
%% 'x(1)'(A,B) :- A>0, C>0, 'x(0)'(C,B).
%% 'x(1)'(A,B) :- A>0, 'new3(1)'(A,B).
%% 'new3(1)'(A,B) :- A>0, C>0, 'x(0)'(C,B).
%% 'new3(1)'(A,B) :- A>0, 'new3(1)'(A,B).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, E>0, 'y(1)'(D,B).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, E>0, 'new4(2)'(A,B,D,E).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, E>0, 'new5(2)'(A,B,C,D,E).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, 'new6(2)'(A,B,C,D).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, E>0, 'new7(2)'(B,C,D,E).
%% 'z(2)'(A,B) :- C>0, D>0, A>0, 'new8(2)'(A,B,C,D).
%% 'z(2)'(A,B) :- C>0, D>0, 'new4(2)'(A,B,D,C).
%% 'z(2)'(A,B) :- C>0, 'new9(2)'(A,B,C).
%% 'new4(2)'(A,B,C,D) :- A>0, 'new6(2)'(A,B,D,C).
%% 'new4(2)'(A,B,C,D) :- A>0, E>0, 'new7(2)'(B,D,C,E).
%% 'new4(2)'(A,B,C,D) :- A>0, 'new8(2)'(A,B,D,C).
%% 'new4(2)'(A,B,C,D) :- A>0, D>0, E>0, 'new4(2)'(A,B,C,E).
%% 'new4(2)'(A,B,C,D) :- A>0, D>0, E>0, 'new5(2)'(A,B,D,C,E).
%% 'new5(2)'(A,B,C,D,E) :- E>0, C>0, 'new4(2)'(A,B,D,E).
%% 'new5(2)'(A,B,C,D,E) :- E>0, 'new5(2)'(A,B,C,D,E).
%% 'new6(2)'(A,B,C,D) :- A>0, 'new6(2)'(A,B,C,D).
%% 'new6(2)'(A,B,C,D) :- A>0, C>0, E>0, 'new4(2)'(A,B,D,E).
%% 'new6(2)'(A,B,C,D) :- A>0, C>0, E>0, 'new5(2)'(A,B,C,D,E).
%% 'new7(2)'(A,B,C,D) :- D>0, B>0, 'y(1)'(C,A).
%% 'new7(2)'(A,B,C,D) :- D>0, 'new7(2)'(A,B,C,D).
%% 'new8(2)'(A,B,C,D) :- A>0, E>0, 'new7(2)'(B,C,D,E).
%% 'new8(2)'(A,B,C,D) :- A>0, 'new8(2)'(A,B,C,D).
%% 'new9(2)'(A,B,C) :- C>0, D>0, 'new4(2)'(A,B,D,C).
%% 'new9(2)'(A,B,C) :- C>0, 'new9(2)'(A,B,C).

