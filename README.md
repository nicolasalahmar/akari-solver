<h1 align='center' >AKARI SOLVER </h1>
<p> this project was intended as a homework for the basics of artificial intelligence course third year in the faculty of information technology engineering - university of damascus</p>

<h4>Prerequisites</h4>
<ul>
<li>the first predicate is the size of the board and in the following rules it is referenced by Cmax (max number of columns) and Rmax (max number of rows).</li>
<li>wall(point(R,C)) denotes a wall in the board (wallnums are also walls).</li>
<li>wallnum(point(R,C),N) denotes a wallnum in the board and N is its number.</li>
<li>lights are denoted by light(point(R,C)) and it is defined dynamic light/1 (the initial board does not contain any lights, lights are added in the solving stage).</li>
</ul>

<p> the utility is comprised of two main rules:<ol><li>testing the board if it is solved (solved).</li><li>solving the board(solve).</li></ol></p>

<p>the solved rule returns a boolean indicating the status of the board.</p>
<p>the solve rule goes through the stages of the solving algorithm and prints the board in every iteration.</p>
<p>the first portion of the code is dedicated for the definition of the board</br>
after that come predefined utility functions that help with the solving and testing of solved boards.</br>

then the three main rules for the testing function.`</br>`
finally the defined rules for the solving functionality.`</p>`

<p>a board is considered solved when three conditions are met</p><ul><li>all cells are hit by a light(all_cells_lit)</li><li>no two lights hit one cell(no_double_light)</li><li>the number of lights on the sides of the wallnum is equal to the wallnum number(light_count_correct)</li>

<p>as of now the tool is able to solve:<ol><li> axiomatic situations where the number of neighbors of a wallnum are equal to the number of the wallnum</li>
<li> isolated cells where all cells have been lit up and the remaining are singular cells across the board</li></ol>

<footer> <h3>Contributors:</h3>
  <ul>
        <li>George Kassar</li>
        <li>George Nigoghosian</li>
        <li>Johny Kodsy</li>
	<li>Rita Rezeq</li>
        <li>Nicolas Al-Ahmar</li>
</ul>
</footer>
