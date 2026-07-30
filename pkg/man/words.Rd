% Maintained manually in man/words.Rd
\name{words}
\alias{words}
\docType{data}
\title{Curated Word List for Games and Examples}
\description{
  A character vector of 110,741 English words suitable for use in word
  games, demonstrations, and package examples. The list was constructed
  to be broad enough for general-purpose use while excluding offensive
  or inappropriate vocabulary, making it safe for classroom and family
  settings.
}
\usage{data(words)}
\format{
  A character vector of length 110,741. All entries are lower-case
  alphabetic strings, sorted in ascending alphabetical order.
}
\details{
  The list was assembled in three steps:

  \enumerate{
    \item The union of three source word lists was formed:
      \itemize{
        \item \file{3of6game.txt} from the \emph{12Dicts} project
          (Alan Beale), a curated list of common English words compiled
          from twelve dictionaries and specifically designed for word
          games.
        \item \file{crosswd.txt} and \file{crswd-d.txt} from Project
          Gutenberg (Category 8: Word Lists), providing an extensive
          crossword-oriented vocabulary.
      }
    \item That union was intersected with \file{safedict_full.txt} from
      the \emph{dict4schools} project (Max Fan / InnovativeInventor),
      which filters out offensive, vulgar, and age-inappropriate terms.
      This intersection step ensures the resulting list is suitable for
      educational and family use.
  }

  Because the list is stored as package data rather than constructed at
  run time, it is identical across sessions and does not depend on the
  availability of the original source files.
}
\source{
  \strong{12Dicts word lists} (Alan Beale):
  \url{http://wordlist.aspell.net/12dicts-readme/}

  The \file{3of6game.txt} file is part of the 12Dicts package, a
  collection of word lists derived from twelve freely available English
  dictionaries. The 12Dicts word lists are released to the public
  domain by their author, Alan Beale.

  \strong{Project Gutenberg word lists} (\file{crosswd.txt} and
  \file{crswd-d.txt}):
  \url{https://www.gutenberg.org/browse/categories/8}

  These crossword word lists are made available by Project Gutenberg.
  Project Gutenberg content in Category 8 (Moby Word Lists) is in the public
  domain.

  \strong{dict4schools safe word list} (\file{safedict_full.txt}):
  Max Fan (InnovativeInventor). \emph{dict4schools}.
  \url{https://github.com/InnovativeInventor/dict4schools}

  The \file{safedict_full.txt} filter list is provided under the MIT
  License. This list was used solely to intersect with (and thereby
  remove inappropriate entries from) the union of the above sources;
  no content unique to dict4schools appears in the final word list.
}
\examples{
data(words)

# Total number of words
length(words)

# Words of exactly 7 letters (useful for word games)
seven <- words[nchar(words) == 7]
length(seven)
head(seven)

# Sample 10 random words
set.seed(42)
sample(words, 10)

# Words beginning with "quant"
grep("^quant", words, value = TRUE)

# Distribution of word lengths
barplot(table(nchar(words)),
        xlab = "Word length", ylab = "Count",
        main = "Distribution of word lengths in 'words'")
}
\keyword{datasets}
