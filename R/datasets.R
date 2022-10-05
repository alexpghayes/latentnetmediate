#' A portion of the AddHealth dataset pulled from an archived UCI site
#'
#' The ADD HEALTH data are constructed from the in-school questionnaire;
#' 90,118 students representing 84 communities took this survey in 1994-95.
#'
#' @details
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
#' Data collected by the Lab for Big Data Methodology at the
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

#' @format A [tidygraph::tbl_graph()] graph object, made up of a node table
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


#' Top level gender-flaired Reddit comments
#'
#' Reddit is an online forum divided into topic-specific subforums called
#' 'subreddits. We consider three subreddits: `keto`, `okcupid`, and
#' `childfree`. In these subreddits, we identify users whose username
#' flair includes a gender label (usually 'M' or 'F'). We collect all top-level
#' comments from these users in 2018. We use each comment's text and score,
#' the number of likes minus dislikes from other users.
#' The dataset includes 90k comments in the selected subreddits.
#'
#' See <https://github.com/blei-lab/causal-text-embeddings> for a replication
#' package for Veitch et al (2020). See <https://github.com/blei-lab/causal-text-embeddings/blob/master/src/reddit/data_cleaning/BigQuery_get_data>
#' in particular for additional data details.
#'
#' @references Veitch, Victor, Dhanya Sridhar, and David M Blei.
#'  "Adapting Text Embeddings for Causal Inference." In Proceedings of the
#'  36 Th Conference on Uncertainty in Artificial Intelligence (UAI),
#'  124:10, 2020.
#'
#' @format A [tidygraph::tbl_graph()] bipartite graph object, made up
#'   of a node table and an edge table.
#'
#'   The node table has columns:
#'
#'   - `type` (logical): A logical indicator of whether a node corresponds to
#'     a word or a reddit post. Used internally by `igraph` -- see `node_type`
#'     for an easier to use alternative.
#'   - `name` (character): Unique node identifier. There is one node in the
#'     graph for each reddit post, and for each word used in a reddit post.
#'     Posts are identified by a number. Words are identified by tokenized
#'     words. Tokenized of raw top-level comment text was performed with
#'     `tidytext::unnest_tokens(..., token = "tweets")`.
#'   - `node_type` (character): Either `"post"` or `"word"`.
#'   - `author_gender`: Either `"female"` or `"male"`, based on user
#'     flairs. Only available for post nodes.
#'   - `score` (integer): The number of upvotes minus the number of downvotes
#'     received by a given post. Only available for post nodes.
#'   - `subreddit` (character): One of `"keto"`, `"okcupid"` or `"childfree"`.
#'     Only available for post nodes.
#'   - `author_pseudonym` (character): An author identifier that is consistent
#'     across posts. Only available for post nodes.
#'
#'   and the edge table has columns:
#'
#'   - `from` (int): Id of post
#'   - `to` (int): Id of word
#'   - `weight` (double): Number of times a word was used in a post.
#'
#' @source Downloaded from <https://archive.org/details/reddit_posts_2018> on
#'   June 6, 2022.
#'
"reddit"

#' Leukemia cases by census tract in New York
#'
#' @references Wakefield, J., and A. Kim. "A Bayesian Model for Cluster
#'   Detection." Biostatistics 14, no. 4 (September 1, 2013):
#'   752–65. https://doi.org/10.1093/biostatistics/kxt001.
#'
#' @format An undirected [tidygraph::tbl_graph()] graph object, made up
#'   of a node table and an edge table.
#'
#'   The node table has columns:
#'
#'   - `population` (numeric): TODO
#'   - `cases` (numeric): TODO
#'   - `censustract_fips` (list of character vectors): TODO
#'
#'   and the edge table has columns:
#'
#'   - `from` (int): Id of source census tract
#'   - `to` (int): Id of neighboring census tract
#'
#' @source Extracted from the `SpatialEpi` package on July 29, 2022. See
#'   [SpatialEpi::NYleukemia] for details, as well as
#'   Wakefield and Kim (2013).
#'
"leukemia"


#' Teenage Friends and Lifestyle Data
#'
#' The canonical "Glasgow" data, as reported in the `siena` R package
#' for network data analysis. `netmediate` simply re-exports this data
#' in a more convenient format. See the `siena` data document for
#' gory details.
#'
#' @references <https://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.htm>
#'
#' @format A list of three [tidygraph::tbl_graph()] directed graphs objects,
#'   representing the same network measured at three different points in
#'   time.
#'
#'   TODO: describe the tidygraphs
#'
#' @source Retrieved from
#' <https://www.stats.ox.ac.uk/~snijders/siena/Glasgow_data.zip> on
#' June 8, 2022.
#'
"glasgow"
