#' A portion of the AddHealth dataset pulled from an archived UCI site
#'
#' Per the original documentation:
#'
#' The ADD HEALTH data are constructed from the in-school questionnaire;
#' 90,118 students representing 84 communities took this survey in 1994-95.
#' Some communities had only one school; others had two. Where there are two
#' schools in a community students from one school were allowed to name
#' friends in the other, the "sister school."
#'
#' Each student was given a paper-and-pencil questionnaire and a copy of a
#' roster listing every student in the school and, if the community had two
#' schools, the student s provided with the roster of the "sister" school.
#' The name generator asked about five male and five female friends separately.
#' The question was, "List your closest (male/female) friends. List your best
#' (male/female) friend first, then your next best friend, and so on.
#' (girls/boys) may include (boys/girls) who are friends and (boy/girl)
#' friends."
#'
#' For each friend named, the student was asked to check off whether he/she
#' participated in any of five activities with the friend. These activities
#' were:
#'
#' 1. you went to (his/her) house in the last seven days.
#' 2. you met (him/her) after school to hang out or go somewhere in the last seven days.
#' 3. you spent time with (him/her) last weekend.
#' 4. you talked with (him/her) about a problem in the last seven days.
#' 5. you talked with (him/her) on the telephone in the last seven days.
#'
#' These activities were summed to create a valued network. Ties range in
#' value from 1, meaning the student nominated the friend but reported no
#' activities, to 6, meaning the student nominated the friend and reported
#' participating in all five activities with the friend.
#'
#' Communities 1 and 48 are omitted. Community 1 is omitted due to data that
#' does not follow the original coding scheme. Community 48 is omitted because
#' the node attribute data was not available online at the time we pulled it.
#'
#' @references Moody, James, "Peer influence groups: identifying dense clusters in large networks," Social Networks, 2001, 23: 261-283.
#'
#' @format A list of [tidygraph::tbl_graph()] graph objects, each corresponding
#'   to one of the original 84 communities.
#'
#'   Each community graph is a directed graph with node table:
#'
#'   - `sex` (factor): Sex of student, coded as `male` or `female`. Possibly missing.
#'   - `race` (factor): Race of student, code as `white`, `black`, `hispanic`, `asian`, or `mixed/other`. Possibly missing.
#'   - `grade` (integer): Grade of student, between 6 and 12. Possibly missing.
#'   - `school` (factor): For communities with sister schools, the first school is coded as factor `A` and the second as `B`.
#'
#'   and edge table:
#'
#'   - `from` (int): Id of student declaring friend
#'   - `to` (int): Id of student declared as friend
#'   - `weight` (int): A number from 1 to 6. See details for an explanation.
#'
#' @source Originally at \url{http://moreno.ss.uci.edu/data.html}, but
#'   ultimately accessed via \url{https://web.archive.org/web/20210115130726/http://moreno.ss.uci.edu/data.html}
#'
"addhealth"


#' Smoking in a network of chinese college students
#'
#' Section 6 of Liu et al. describes the data as follows:
#'
#' We use the data collected by the Lab for Big Data Methodology at the
#' University of Notre Dame in 2017. The participants were 162 students in a
#' 4-year college in China. There were 90 female and 72 male students.
#' Their average age was 21.64 years (SD = 0.86). During the data collection,
#' each student indicated whether the other students were his/her friends
#' or not. We symmetrized the friendship network using the strongest relation
#' (i.e., if one of two students indicated the other as a friend,
#' we assumed they were mutual friends).
#'
#' @references Liu, Haiyan, Ick Hoon Jin, Zhiyong Zhang, and Ying Yuan.
#'    "Social Network Mediation Analysis: A Latent Space Approach."
#'    Psychometrika 86, no. 1 (March 2021): 272–98.
#'    https://doi.org/10.1007/s11336-020-09736-z.

#' @format A [tidygraph::tbl_graph()] graph objects, made up of a node table
#'   and an edge table.
#'
#'   The node table has columns:
#'
#'   - `sex` (factor): Sex of student, coded as `male` or `female`.
#'   - `smokes` (factor): Whether student smokes. Levels are `smokes` and `doesn't smoke`.
#'
#'   and the edge table has columns:
#'
#'   - `from` (int): Id of student declaring friend
#'   - `to` (int): Id of student declared as friend
#'
#' @source Obtained from Haiyan Liu over email on April 28, 2022.
#'
"smoking"
