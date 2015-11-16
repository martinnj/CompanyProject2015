# Glossary
TDIDT = Top-Down Induction of Decision Trees
CLS = Hunt's Concept Learning System framework.
ACL = Method developed from CLS.


# Training Sets

Trees can be built based on 2 types of data sets:
    1) Entries recorded over a period of time, or
    2) A set of entries constructed by a domain expert.

Each set have advantages and downsides, the first one will be statistically
sound and reflect the most common cases, but it may not capture rare incidents
that have not happend during the time of recording, such things could easilier
be included in a tutorial dataset constructed by an expert. Furthermore, the
first type might include a lot of redundant data, which will all need to be
parsed.


# The Induction Task

The basis is a universe of *objects* which all have a collection of
*attributes*, these attributes should measure some important feature and works
best if limited to some finite set of values.

Each object belongs to one of a set of mutually exclusive *classes*.

If there training set contains samples with the same attributes, but different
classifications, the attributes for the entire object is called *inadequate*.

Many training sets can produce more than one decision tree that is correct for
the training data available, but the goal is to create a tree that is able to
correctly classify objects from outside the training set. Thus, the *simpler*
tree should be used, since larger and more complicated trees makes it suspect as
an *explanation*.


# ID3

*ID3* is a method for constructing decision trees that are reasonably good, for
many objects with a large number of attributes. ID3 cannot guarantee that a
better tree have not been overlooked though.

The method is iterative:
    1) A *window* is chosen from the training set (that is, a number of
       objects.)
    2) A decision tree is formed that correctly classifies the objects in the
       window.
    3) The tree is used to classify the remaining objects
        - If it correctly classigies them all, a suitable tree have been mound.
        - If it misclasifies one or more objects, these objects are added to the
          *window*, and the process starts from step 2) again.

This method is found to produce trees after only a few iterations, with training
data containing 30.000 objects with 50 attributes.

Details are on page 88 (8 in the pdf)