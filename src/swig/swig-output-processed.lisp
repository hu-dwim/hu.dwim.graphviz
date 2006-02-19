(in-package :graphviz)


;;;SWIG wrapper code starts here

(defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(progn ,@(loop for value in enums
                  for index = 0 then (1+ index)
                  when (listp value) do (setf index (second value)
                                              value (first value))
                  collect `(defconstant ,value ,index))))

;;;SWIG wrapper code ends here


(defcfun ("gvToggle" gvToggle) :void
  (arg0 :int))

(defcfun ("next_input_graph" next_input_graph) :pointer)

(defcfun ("gvNEWcontext" gvNEWcontext) :pointer
  (info :pointer)
  (user :string))

(defcfun ("gvUsername" gvUsername) :string)

(defcfun ("gvContext" gvContext) :pointer)

(defcfun ("gvParseArgs" gvParseArgs) :int
  (gvc :pointer)
  (argc :int)
  (argv :pointer))

(defcfun ("gvLayout" gvLayout) :int
  (gvc :pointer)
  (g :pointer)
  (engine :string))

(defcfun ("gvLayoutJobs" gvLayoutJobs) :int
  (gvc :pointer)
  (g :pointer))

(defcfun ("attach_attrs" attach_attrs) :void
  (g :pointer))

(defcfun ("agstrdup_html" agstrdup_html) :string
  (s :string))

(defcfun ("aghtmlstr" aghtmlstr) :int
  (s :string))

(defcfun ("gvRender" gvRender) :int
  (gvc :pointer)
  (g :pointer)
  (format :string)
  (out :pointer))

(defcfun ("gvRenderFilename" gvRenderFilename) :int
  (gvc :pointer)
  (g :pointer)
  (format :string)
  (filename :string))

(defcfun ("gvRenderJobs" gvRenderJobs) :int
  (gvc :pointer)
  (g :pointer))

(defcfun ("gvFreeLayout" gvFreeLayout) :int
  (gvc :pointer)
  (g :pointer))

(defcfun ("gvFreeContext" gvFreeContext) :int
  (gvc :pointer))

(defconstant _GRAPH_H 1)

(defconstant TAIL_ID "tailport")

(defconstant HEAD_ID "headport")

(defconstant AGFLAG_DIRECTED 1)

(defconstant AGFLAG_STRICT 2)

(defconstant AGFLAG_METAGRAPH 4)

(defconstant AGRAPH 0)

(defconstant AGRAPHSTRICT 2)

(defconstant AGDIGRAPH 1)

(defconstant AGDIGRAPHSTRICT 3)

(defconstant AGMETAGRAPH 7)

(defcstruct Agraph_t
	(tag :int)
	(kind :int)
	(handle :int)
	(attr :pointer)
	(name :string)
	(univ :pointer)
	(nodes :pointer)
	(inedges :pointer)
	(outedges :pointer)
	(root :pointer)
	(meta_node :pointer)
	(proto :pointer)
	(u :pointer))

(defcstruct Agnode_t
	(tag :int)
	(pad :int)
	(handle :int)
	(attr :pointer)
	(name :string)
	(id :int)
	(graph :pointer)
	(u :pointer))

(defcstruct Agedge_t
	(tag :int)
	(printkey :int)
	(handle :int)
	(attr :pointer)
	(head :pointer)
	(tail :pointer)
	(id :int)
	(u :pointer))

(defcstruct Agdata_t
	(node_dict :pointer)
	(nodeattr :pointer)
	(edgeattr :pointer)
	(globattr :pointer)
	(max_node_id :int)
	(max_edge_id :int))

(defcstruct Agsym_t
	(name :string)
	(value :string)
	(index :int)
	(printed :unsigned-char)
	(fixed :unsigned-char))

(defcstruct Agdict_t
	(name :string)
	(dict :pointer)
	(list :pointer))

(defcstruct Agproto_t
	(n :pointer)
	(e :pointer)
	(prev :pointer))

(defcfun ("agstrcanon" agstrcanon) :string
  (arg0 :string)
  (arg1 :string))

(defcfun ("agcanonical" agcanonical) :string
  (arg0 :string))

(defcfun ("agget" agget) :string
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agxget" agxget) :string
  (arg0 :pointer)
  (arg1 :int))

(defcfun ("agset" agset) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(defcfun ("agsafeset" agsafeset) :int
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string)
  (arg3 :string))

(defcfun ("agxset" agxset) :int
  (arg0 :pointer)
  (arg1 :int)
  (arg2 :string))

(defcfun ("agindex" agindex) :int
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("aginitlib" aginitlib) :void
  (arg0 :int)
  (arg1 :int)
  (arg2 :int))

(defcfun ("agopen" agopen) :pointer
  (arg0 :string)
  (arg1 :int))

(defcfun ("agsubg" agsubg) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agfindsubg" agfindsubg) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agclose" agclose) :void
  (arg0 :pointer))

(defcfun ("agread" agread) :pointer
  (arg0 :pointer))

(defcfun ("agread_usergets" agread_usergets) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agreadline" agreadline) :void
  (arg0 :int))

(defcfun ("agsetfile" agsetfile) :void
  (arg0 :string))

(defcfun ("agmemread" agmemread) :pointer
  (arg0 :string))

(defcfun ("agwrite" agwrite) :int
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agerrors" agerrors) :int)

(defcfun ("agprotograph" agprotograph) :pointer)

(defcfun ("agusergraph" agusergraph) :pointer
  (arg0 :pointer))

(defcfun ("agnnodes" agnnodes) :int
  (arg0 :pointer))

(defcfun ("agnedges" agnedges) :int
  (arg0 :pointer))

(defcfun ("aginsert" aginsert) :void
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agdelete" agdelete) :void
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agcontains" agcontains) :int
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agnode" agnode) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agnodeattr" agnodeattr) :pointer
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(defcfun ("agfindnode" agfindnode) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agfstnode" agfstnode) :pointer
  (arg0 :pointer))

(defcfun ("agnxtnode" agnxtnode) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("aglstnode" aglstnode) :pointer
  (arg0 :pointer))

(defcfun ("agprvnode" agprvnode) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agedge" agedge) :pointer
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(defcfun ("agedgeattr" agedgeattr) :pointer
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(defcfun ("agfindedge" agfindedge) :pointer
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(defcfun ("agfstedge" agfstedge) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agnxtedge" agnxtedge) :pointer
  (arg0 :pointer)
  (arg1 :pointer)
  (arg2 :pointer))

(defcfun ("agfstin" agfstin) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agnxtin" agnxtin) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agfstout" agfstout) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agnxtout" agnxtout) :pointer
  (arg0 :pointer)
  (arg1 :pointer))

(defcfun ("agraphattr" agraphattr) :pointer
  (arg0 :pointer)
  (arg1 :string)
  (arg2 :string))

(defcfun ("agfindattr" agfindattr) :pointer
  (arg0 :pointer)
  (arg1 :string))

(defcfun ("agcopyattr" agcopyattr) :int
  (arg0 :pointer)
  (arg1 :pointer))

(defcenum agerrlevel_t
	:AGWARN
	:AGERR
	:AGMAX
	:AGPREV)

(defcvar ("agerrno" agerrno)
 :pointer)

(defcfun ("agseterr" agseterr) :void
  (arg0 :pointer))

(defcfun ("aglasterr" aglasterr) :string)

(defcfun ("agerr" agerr) :int
  (level :pointer)
  (fmt :string)
  )

(defcfun ("agerrorf" agerrorf) :void
  (fmt :string)
  )

(defcfun ("agwarningf" agwarningf) :void
  (fmt :string)
  )

(defcfun ("agstrdup" agstrdup) :string
  (arg0 :string))

(defcfun ("agstrfree" agstrfree) :void
  (arg0 :string))

(defcenum agobjkind_t
	(:AGNODE 1)
	:AGEDGE
	:AGGRAPH)

(defcunion inside_t
	(s :pointer)
	(a :pointer))

(defcstruct inside_t_s
	(n :pointer)
	(bp :pointer))

(defcstruct inside_t_a
	(p :pointer)
	(r :pointer))

(defcstruct port
	(p :pointer)
	(theta :double)
	(bp :pointer)
	(defined :pointer)
	(constrained :pointer)
	(clip :pointer)
	(order :unsigned-char)
	(side :unsigned-char))

(defcstruct splineInfo
	(swapEnds :pointer)
	(splineMerge :pointer))

(defcstruct pathend_t
	(nb :pointer)
	(np :pointer)
	(sidemask :int)
	(boxn :int)
	(boxes :pointer))

(defcstruct path
	(start :pointer)
	(end :pointer)
	(ulpp :pointer)
	(urpp :pointer)
	(llpp :pointer)
	(lrpp :pointer)
	(nbox :int)
	(boxes :pointer)
	(data :pointer))

(defcstruct bezier
	(list :pointer)
	(size :int)
	(sflag :int)
	(eflag :int)
	(sp :pointer)
	(ep :pointer))

(defcstruct splines
	(list :pointer)
	(size :int)
	(bb :pointer))

(defcstruct bezierf
	(list :pointer)
	(size :int)
	(sflag :int)
	(eflag :int)
	(sp :pointer)
	(ep :pointer))

(defcstruct splinesf
	(list :pointer)
	(size :int))

(defcstruct textline_t
	(str :string)
	(xshow :string)
	(width :double)
	(just :char))

(defcstruct textlabel_t
	(text :string)
	(fontname :string)
	(fontcolor :string)
	(fontsize :double)
	(dimen :pointer)
	(p :pointer)
	(d :pointer)
	(set :pointer)
	(html :pointer)
	(u :pointer))

(defcunion textlabel_t_u
	(html :pointer)
	(txt :pointer))

(defcstruct textlabel_t_u_txt
	(line :pointer)
	(nlines :short))

(defcstruct polygon_t
	(regular :int)
	(peripheries :int)
	(sides :int)
	(orientation :double)
	(distortion :double)
	(skew :double)
	(option :int)
	(vertices :pointer))

(defcstruct stroke_t
	(nvertices :int)
	(flags :int)
	(vertices :pointer))

(defconstant STROKE_CLOSED 1)

(defconstant STROKE_FILLED 2)

(defconstant STROKE_PENDOWN 4)

(defconstant STROKE_VERTICES_ALLOCATED 8)

(defcstruct shape_t
	(nstrokes :int)
	(strokes :pointer))

(defcstruct shape_functions
	(initfn :pointer)
	(freefn :pointer)
	(portfn :pointer)
	(insidefn :pointer)
	(pboxfn :pointer)
	(codefn :pointer))

(defcenum shape_kind
	:SH_UNSET
	:SH_POLY
	:SH_RECORD
	:SH_POINT
	:SH_EPSF
	:SH_USER)

(defcstruct shape_desc
	(name :string)
	(fns :pointer)
	(polygon :pointer)
	(usershape :pointer))

(defcstruct codegen_s
	(reset :pointer)
	(begin_job :pointer)
	(end_job :pointer)
	(begin_graph :pointer)
	(end_graph :pointer)
	(begin_page :pointer)
	(end_page :pointer)
	(begin_layer :pointer)
	(end_layer :pointer)
	(begin_cluster :pointer)
	(end_cluster :pointer)
	(begin_nodes :pointer)
	(end_nodes :pointer)
	(begin_edges :pointer)
	(end_edges :pointer)
	(begin_node :pointer)
	(end_node :pointer)
	(begin_edge :pointer)
	(end_edge :pointer)
	(begin_context :pointer)
	(end_context :pointer)
	(begin_anchor :pointer)
	(end_anchor :pointer)
	(set_font :pointer)
	(textline :pointer)
	(set_pencolor :pointer)
	(set_fillcolor :pointer)
	(set_style :pointer)
	(ellipse :pointer)
	(polygon :pointer)
	(beziercurve :pointer)
	(polyline :pointer)
	(bezier_has_arrows :pointer)
	(comment :pointer)
	(textsize :pointer)
	(user_shape :pointer)
	(usershapesize :pointer))

(defcstruct codegen_info_s
	(cg :pointer)
	(name :string)
	(id :int)
	(info :pointer))

(defcstruct nodequeue
	(store :pointer)
	(limit :pointer)
	(head :pointer)
	(tail :pointer))

(defcstruct adjmatrix_t
	(nrows :int)
	(ncols :int)
	(data :string))

(defcstruct rank_t
	(n :int)
	(v :pointer)
	(an :int)
	(av :pointer)
	(ht1 :int)
	(ht2 :int)
	(pht1 :int)
	(pht2 :int)
	(candidate :pointer)
	(valid :pointer)
	(cache_nc :int)
	(flat :pointer))

(defcenum ratio_t
	(:R_NONE 0)
	:R_VALUE
	:R_FILL
	:R_COMPRESS
	:R_AUTO
	:R_EXPAND)

(defcstruct layout_t
	(quantum :double)
	(scale :double)
	(ratio :double)
	(dpi :double)
	(margin :pointer)
	(page :pointer)
	(size :pointer)
	(filled :pointer)
	(landscape :pointer)
	(centered :pointer)
	(ratio_kind :pointer))

(defcstruct field_t
	(size :pointer)
	(b :pointer)
	(n_flds :int)
	(lp :pointer)
	(fld :pointer)
	(id :string)
	(LR :unsigned-char)
	(sides :unsigned-char))

(defcstruct nlist_t
	(list :pointer)
	(size :int))

(defcstruct elist
	(list :pointer)
	(size :int))

(defconstant GUI_STATE_ACTIVE 1)

(defconstant GUI_STATE_SELECTED 2)

(defconstant GUI_STATE_VISITED 4)

(defconstant GUI_STATE_DELETED 8)

(defcstruct Agraphinfo_t
	(drawing :pointer)
	(label :pointer)
	(bb :pointer)
	(border :pointer)
	(gui_state :unsigned-char)
	(has_labels :pointer)
	(has_images :pointer)
	(charset :unsigned-char)
	(rankdir :int)
	(ht1 :int)
	(ht2 :int)
	(flags :unsigned-short)
	(alg :pointer)
	(gvc :pointer)
	(cleanup :pointer)
	(neato_nlist :pointer)
	(move :int)
	(dist :pointer)
	(spring :pointer)
	(sum_t :pointer)
	(t_ :pointer)
	(ndim :int)
	(n_cluster :int)
	(clust :pointer)
	(nlist :pointer)
	(rank :pointer)
	(comp :pointer)
	(minset :pointer)
	(maxset :pointer)
	(n_nodes :long)
	(minrank :short)
	(maxrank :short)
	(has_flat_edges :pointer)
	(showboxes :pointer)
	(cluster_was_collapsed :pointer)
	(nodesep :int)
	(ranksep :int)
	(ln :pointer)
	(rn :pointer)
	(leader :pointer)
	(rankleader :pointer)
	(expanded :pointer)
	(installed :char)
	(set_type :char)
	(label_pos :char)
	(exact_ranksep :pointer))

(defcstruct Agnodeinfo_t
	(shape :pointer)
	(shape_info :pointer)
	(coord :pointer)
	(width :double)
	(height :double)
	(bb :pointer)
	(ht :int)
	(lw :int)
	(rw :int)
	(label :pointer)
	(alg :pointer)
	(state :char)
	(gui_state :unsigned-char)
	(clustnode :pointer)
	(pinned :pointer)
	(xsize :short)
	(ysize :short)
	(id :int)
	(heapindex :int)
	(hops :int)
	(pos :pointer)
	(dist :double)
	(showboxes :pointer)
	(has_port :pointer)
	(node_type :char)
	(mark :char)
	(onstack :char)
	(ranktype :char)
	(weight_class :char)
	(next :pointer)
	(prev :pointer)
	(in :pointer)
	(out :pointer)
	(flat_out :pointer)
	(flat_in :pointer)
	(other :pointer)
	(clust :pointer)
	(UF_size :int)
	(UF_parent :pointer)
	(inleaf :pointer)
	(outleaf :pointer)
	(rank :int)
	(order :int)
	(mval :int)
	(save_in :pointer)
	(save_out :pointer)
	(tree_in :pointer)
	(tree_out :pointer)
	(par :pointer)
	(low :int)
	(lim :int)
	(priority :int)
	(pad :pointer))

(defcstruct Agedgeinfo_t
	(spl :pointer)
	(tail_port :pointer)
	(head_port :pointer)
	(label :pointer)
	(head_label :pointer)
	(tail_label :pointer)
	(edge_type :char)
	(adjacent :char)
	(label_ontop :char)
	(gui_state :unsigned-char)
	(to_orig :pointer)
	(alg :pointer)
	(factor :double)
	(dist :double)
	(path :pointer)
	(showboxes :pointer)
	(conc_opp_flag :pointer)
	(xpenalty :short)
	(weight :int)
	(cutvalue :int)
	(tree_index :int)
	(count :short)
	(minlen :unsigned-short)
	(to_virt :pointer))

(defcstruct fdpParms_t
	(useGrid :int)
	(useNew :int)
	(numIters :int)
	(unscaled :int)
	(C :double)
	(Tfact :double)
	(K :double)
	(T0 :double))

(defcstruct point
	(x :int)
	(y :int))

(defcstruct pointf
	(x :double)
	(y :double))

(defcstruct box
	(LL :pointer)
	(UR :pointer))

(defcstruct boxf
	(LL :pointer)
	(UR :pointer))

(defconstant POINTS_PER_INCH 72)


