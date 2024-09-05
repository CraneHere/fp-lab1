# FP: Lab 1. Вычисляем функционально

## Учимся решать вычислительные задачи в функциональном стиле

**Цель работы:** Учимся работать с системой программирования и мыслить в функциональном стиле  

## Задание 1: Ряд Тейлора 

Напишите программу на F#, которая распечатает значения некоторой математической функции **f(x)**, заданной вариантом задания в соответствии с [таблицей](Lab1.pdf), на заданном (в таблице) интервале **[a,b]**. Функцию необходимо вычислить тремя способами:

 * С помощью встроенных функций F#
 * Используя **наивный способ** вычисления ряда Тейлора, где каждый член ряда вычисляется по формуле
 * Используя **умный способ** вычисления ряда Тейлора, где каждый следующий член вычисляется на основе предыдущего.

В двух последних случаях, вам необходимо осуществлять суммирование членов до тех пор, пока их абсолютное значение не станет меньше некоторого заданного небольшого значения **eps**. Необходимое число членов суммы также необходимо напечатать в таблице.

Вот как будет выглядеть таблица (вариант 7):

|  x  |   Builtin  | Smart Taylor | # terms | Dumb Taylor | # terms |
|-----|------------|--------------|---------|-------------|---------|
| 0.00|   0.000000 |    0.000000  |     1   |    0.000000 |     1   |
| 0.05|   0.172037 |    0.172036  |     5   |    0.172036 |     6   |
| 0.10|   0.397805 |    0.397798  |     6   |    0.397798 |     7   |
| 0.15|   0.696112 |    0.696108  |     8   |    0.696108 |     9   |
| 0.20|   1.093750 |    1.093746  |    10   |    1.093746 |    11   |
| 0.25|   1.629630 |    1.629626  |    12   |    1.629626 |    13   |
| 0.30|   2.361516 |    2.361511  |    14   |    2.361511 |    15   |
| 0.35|   3.377333 |    3.377323  |    16   |    3.377323 |    17   | 
| 0.40|   4.814815 |    4.814806  |    19   |    4.814806 |    20   |
| 0.45|   6.897070 |    6.897058  |    22   |    6.897058 |    23   |
| 0.50|  10.000000 |    9.999987  |    26   |    9.999987 |    27   |

## Задание 2: Численное решение трансцендентных уравнений

Реализуйте функцию для численного решения трансцендентных алгебраических уравнений, используя следующие [алгоритмы нахождения корней](https://en.wikipedia.org/wiki/Root-finding_algorithms): 

 * [Дихотомия](https://en.wikipedia.org/wiki/Bisection_method)
 * [Метод итераций](http://www.simumath.com/library/book.html?code=Alg_Equations_Iterations)
 * [Метод Ньютона](https://en.wikipedia.org/wiki/Newton%27s_method)

Необходимо разработать функции для решения произвольных уравнений, которые передаются как параметр-функция. Используйте также тот факт, что метод Ньютона является частным случаем метода итераций. 

Примените три функции решения к трем последовательным уравнениям [из таблицы](Lab1.pdf), начиная со своего варианта задания. У вас должна получиться табличка из 9 решений (3 уравнения по 3 метода на каждое).

1)  $3 * \ln^2 x + 6*\ln^2 x - 5 = 0$
2)  $0.6 * 3^x - 2.3 * x - 3 = 0$
3)  $x^2 - \ln (1 + x) - 3 = 0$

| № | dichotomy | iterations | newton | 
|---|---------|--------------|---------|
| 1 |    1.88324 |    1.88324 |    1.88324|
| 2 |   2.41998  |    2.41998 |    2.41998|
| 3 |   2.02669  |    2.02669 |    2.02669|

