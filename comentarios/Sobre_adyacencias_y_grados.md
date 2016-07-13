Buenas Loles

He recopilado algunos ejemplos sobre adyacencia en distintos sistemas.

En [Maxima](http://bit.ly/29hKv3u)

~~~
(%i1) load (graphs)$

(%i2) p : path_digraph(3)$

(%i3) p;
(%o3)                     DIGRAPH(3 vertices, 2 arcs)
(%i4) adjacency_matrix (p);
                                  [ 0  1  0 ]
                                  [         ]
(%o4)                             [ 0  0  1 ]
                                  [         ]
                                  [ 0  0  0 ]
(%i5) in_neighbors(2, p);
(%o5)                                 [1]
(%i6) out_neighbors(2, p);
(%o6)                                 []
~~~

Sobre grados en no dirigidos

~~~
(%i16) g1 : create_graph ([1,2,3], [[1,2],[3,1]]);
(%o16)                    GRAPH(3 vertices, 2 edges)
(%i17) max_degree(g1);
(%o17)                              [2, 1]
(%i18) min_degree(g1);
(%o18)                              [1, 2]
(%i19) vertex_degree(1, g1);
(%o19)                                 2
(%i20) vertex_degree(2, g1);
(%o20)                                 1
(%i21) vertex_degree(3, g1);
(%o21)                                 1

(%i22) g2 : create_graph ([1,2,3], [[1,2],[3,1]], directed=true);
(%o22)                    DIGRAPH(3 vertices, 2 arcs)
(%i23) max_degree(g2);

Argument 1 to `MAX_DEGREE' is not a graph: DIGRAPH(3 vertices, 2 arcs)
 -- an error. To debug this try: debugmode(true);
(%i24) vertex_degree(1,g2);

Argument 2 to `VERTEX_DEGREE' is not a graph: DIGRAPH(3 vertices, 2 arcs)
 -- an error. To debug this try: debugmode(true);

(%i35) g2 : create_graph ([1,2,3], [[1,2],[3,1]], directed=true);
(%o35)                    DIGRAPH(3 vertices, 2 arcs)
(%i36) vertex_degree (1,g2);

Argument 2 to `VERTEX_DEGREE' is not a graph: DIGRAPH(3 vertices, 2 arcs)
 -- an error. To debug this try: debugmode(true);
(%i37) vertex_in_degree (1,g2);
(%o37)                                 1
(%i38) vertex_out_degree (1,g2);
(%o38)                                 1
~~~

Sobre entornos

~~~
(%i27) g1 : create_graph ([1,2,3], [[1,2],[3,1]]);
(%o27)                    GRAPH(3 vertices, 2 edges)
(%i28) neighbors (1, g1);
(%o28)                              [3, 2]
(%i29) g2 : create_graph ([1,2,3], [[1,2],[3,1]], directed=true);
(%o29)                    DIGRAPH(3 vertices, 2 arcs)
(%i30) neighbors (1, g2);

Argument 2 to `NEIGHBORS' is not a graph: DIGRAPH(3 vertices, 2 arcs)
 -- an error. To debug this try: debugmode(true);
(%i31) out_neighbors (1, g2);
(%o31)                                [2]
(%i32) in_neighbors (1, g2);
(%o32)                                [3]
~~~

En Mathematica puedes ver las funciones
[VertexInDegree](http://bit.ly/29hQ2Hi),
[VertexOutDegree](http://bit.ly/29hPpO8) y
[VertexDegree](http://bit.ly/29hPBwN).

Saludos, José A.
