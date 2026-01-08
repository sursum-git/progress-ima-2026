RUN emptyRowErrors   IN {1}.
RUN setRecord        IN {1}(INPUT TABLE {2}).
RUN createRecord     IN {1}.
RUN getRowErrors      IN {1}(OUTPUT TABLE RowErrors).


