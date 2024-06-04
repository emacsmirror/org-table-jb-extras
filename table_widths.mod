# AMPL program for finding the column widths required to fit a table of text into a given width.

#if ($AMPL_SOLVER) = "" then { option solver bonmin } else {option solver ($AMPL_SOLVER)};
option solver gurobi; # gurobi is fastest for this problem

param ncols;  # Number of columns
param nrows;  # Number of rows
param maxwidth;  # Specified width of the table
param cell_lengths{i in 1..nrows, j in 1..ncols};  # Length of the content in each cell

# Decision variables
var rows{i in 1..nrows} >= 1, integer;  # Extra rows added for each original row
var widths{j in 1..ncols} >= 1, integer;  # Width of each column

# Objective: Minimize the total number of rows
minimize total_rows: sum {i in 1..nrows} rows[i];

# Constraints
subject to total_width:
    sum {j in 1..ncols} widths[j] <= maxwidth;

# THIS IS NONLINEAR!
subject to fit_cells {i in 1..nrows, j in 1..ncols}:
	cell_lengths[i, j] <= widths[j]*rows[i];

#data table_data.dat;
#data /tmp/orgtbl.dat;
read nrows, ncols, maxwidth < -;
read {i in 1..nrows, j in 1..ncols} cell_lengths[i,j] < -;

solve >/dev/null;

# Display the results
#printf "Rows| Col widths:   "; print {j in 1..ncols} widths[j]; printf {i in 1..nrows} "%d   |\n", rows[i];
printf "Widths:";
printf {i in 1..ncols} " %d", widths[i];
printf "\n";
printf "Rows:";
printf {j in 1..nrows} " %d", rows[j];
printf "\n";
#display widths, rows, total_rows;
#display total_rows;
end;
