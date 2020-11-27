install.packages("rcompanion")


x <- matrix(c(76, 22, 45, 30, 11, 45, 40, 6, 25, 36, 10, 40), byrow=TRUE, nrow=4, ncol=3);
fisher.test(x);


Input =("
 Guidelines   None_declared   No_mention  Declared_any 
 1            76                22        45
 2            30                11        45
 3            40                6         25
 4            36                10        40
")

Matriz=as.matrix(read.table(textConnection(Input),
                            header=TRUE,
                            row.names=1))

fisher.test(workspace=4000000, Matriz,
            alternative = "two.sided")

Input2=("
 Reference   None_declared   No_mention  Declared_any 
 5            43                17        34
 6            28                2         44
 7            13                4         31
 8            40                16        26
 9            58                10        20
")

reference=as.matrix(read.table(textConnection(Input2),
                            header=TRUE,
                            row.names=1))

fisher.test(simulate.p.value=TRUE, reference,
            alternative = "two.sided")
