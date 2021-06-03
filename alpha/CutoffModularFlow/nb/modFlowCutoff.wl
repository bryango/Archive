#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Subsubsection:: *)
(*Synopsis - Cutoff*)


(* ::Text:: *)
(*This notebook:*)
(* - calculates the modular flow of an interval in cutoff Poincar\[EAcute] Subscript[AdS, 3]*)
(* - ... based on the results of modFlowPoincare.nb*)
(* - We then compute the flow lines for the cutoff modular flow*)


(* ::Subsubsection:: *)
(*Setup*)


(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[];
<<"Physica/MathUtils.wl"


(* ::Input::Initialization:: *)
<<Notation`
Symbolize[ParsedBoxWrapper[SubscriptBox["_", "_"]]];


(* ::Input::Initialization:: *)
dX[n_]:=Symbol["d"<>ToString[coord[[n]]]]


(* ::Input::Initialization:: *)
$Assumptions=And[u\[Element]Reals,v\[Element]Reals,z>=0,zc>0,L>0,Linf>0,\[Tau]\[Element]Reals,\[Theta]\[Element]Reals,r>Tu Tv,Tu>0,Tv>0,r>T^2,T>0,r>0,r^2>T^4];


(* ::Input::Initialization:: *)
coord={u,v,z};
dim=Length[coord];
metricSign=-1;

ds2=(+du dv+dz^2)/z^2;

metric=Table[1/2 D[ds2,dX[i],dX[j]],{i,1,dim},{j,1,dim}]
<<diffgeoM`


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Einstein- metric*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*("DefaultPlotStyle"/.(Method/.Charting`ResolvePlotTheme[#2,#1]))/.Directive[x_,__]:>x&[Plot,"Web"]*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Killing vectors*)


(* ::Text:: *)
(*Bulk Killings matched with the boundary Killings:*)


(* ::Input::Initialization:: *)
ju=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "1"}], ",", "0", ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", "u"}], ",", "0", ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
RowBox[{"-", 
SuperscriptBox["u", "2"]}], ",", 
SuperscriptBox["z", "2"], ",", 
RowBox[{
RowBox[{"-", "u"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];


(* ::Input::Initialization:: *)
jv=Function[{n},\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "1"}], ",", "0"}], "}"}], 
RowBox[{"n", "==", 
RowBox[{"-", "1"}]}]},
{
RowBox[{"{", 
RowBox[{"0", ",", 
RowBox[{"-", "v"}], ",", 
RowBox[{"-", 
FractionBox["z", "2"]}]}], "}"}], 
RowBox[{"n", "==", "0"}]},
{
RowBox[{"{", 
RowBox[{
SuperscriptBox["z", "2"], ",", 
RowBox[{"-", 
SuperscriptBox["v", "2"]}], ",", 
RowBox[{
RowBox[{"-", "v"}], " ", "z"}]}], "}"}], 
RowBox[{"n", "==", "1"}]},
{"0", 
TagBox["True",
"PiecewiseDefault",
AutoDelete->True]}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "Rows" -> {{Baseline}}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "Rows" -> {{1.}}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)];


(* ::Text:: *)
(**)


(* ::Text::Closed:: *)
(*Check Killing's equation:*)


(* ::Input:: *)
(*lieD[ju[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(*lieD[jv[#1],metric]&/@Range[-1,1]*)


(* ::Input:: *)
(**)


(* ::Text::Closed:: *)
(*Check the algebra: [Subscript[L, m],Subscript[L, n]]=(m-n)Subscript[L, m+n]*)


(* ::Input:: *)
(*Subsets[{-1,0,1},{2}]*)


(* ::Input:: *)
(*( *)
(*lieD[ju[#1],ju[#2],{up}]*)
(*-(#1-#2)ju[#1+#2]*)
(*)&@@@Subsets[{-1,0,1},{2}]//Simplify*)


(* ::Input:: *)
(*( *)
(*lieD[jv[#1],jv[#2],{up}]*)
(*-(#1-#2)jv[#1+#2]*)
(*)&@@@Subsets[{-1,0,1},{2}]//Simplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*lieD[ju[#1],jv[#2],{up}]&@@@Tuples[{-1,0,1},2]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(**)


(* ::Subsubsection::Closed:: *)
(*Derivation of bulk mod flow*)


(* ::Input::Initialization:: *)
coeffNames={lm,l0,lp,Lm,L0,Lp};

\[Chi]def=HoldForm[2\[Pi](
{lm,l0,lp}.(ju/@Range[-1,1])
-{Lm,L0,Lp}.(jv/@Range[-1,1])
)];

\[Chi]=FullSimplify[ReleaseHold[\[Chi]def]]


(* ::Input:: *)
(*lieD[\[Chi],metric]*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
uv2xt={u->x+t,v->x-t};
xt2uv=Solve[uv2xt/.Rule->Equal,{x,t}]//First


(* ::Input:: *)
(**)


(* ::Text:: *)
(*RT surface:*)


(* ::Input::Initialization:: *)
rtSurfaceGeneral=Function[{t,x,z},
(z^2+(x-(SubPlus[x]+SubMinus[x])/2)^2-(t-(SubPlus[t]+SubMinus[t])/2)^2)
-(((SubPlus[x]-SubMinus[x])/2)^2-((SubPlus[t]-SubMinus[t])/2)^2)
];

rtSurfaceLightcone=Function[{u,v,z},
rtSurfaceGeneral[t,x,z]/.xt2uv
/.(xt2uv/.{x->SubMinus[x],t->SubMinus[t],u->SubMinus[u],v->SubMinus[v]})
/.(xt2uv/.{x->SubPlus[x],t->SubPlus[t],u->SubPlus[u],v->SubPlus[v]})//Simplify//Evaluate
]


(* ::Input::Initialization:: *)
length2uv=(l->Sqrt[(SubPlus[u]-SubMinus[u]) (SubPlus[v]-SubMinus[v])]);


(* ::Input:: *)
(**)


(* ::Item:: *)
(*General RT surface:*)


(* ::Text:: *)
(*Modular flow should vanish along the RT surface:*)


(* ::Input::Initialization:: *)
rtZ2uv=(z^2->-(((u-SubPlus[u])(v-SubMinus[v])+(u-SubMinus[u])(v-SubPlus[v]))/2))


(* ::Input:: *)
(*rtSurfaceLightcone[u,v,z]/.rtZ2uv//Simplify*)


(* ::Input::Initialization:: *)
Thread[Sqrt[rtZ2uv],Rule]//Simplify

rtZuv=%;


(* ::Text:: *)
(*Also: u,v linear along the RT surface!*)


(* ::Input::Initialization:: *)
rtV2U=(v->(SubPlus[v]+SubMinus[v])/2+(SubPlus[v]-SubMinus[v])/(SubPlus[u]-SubMinus[u]) (u-(SubPlus[u]+SubMinus[u])/2)//FullSimplify);


(* ::Input::Initialization:: *)
Collect[\[Chi]//.rtZ2uv/.rtV2U,u,FullSimplify]//Column[#,Frame->All]&;

CoefficientList[\[Chi]//.rtZ2uv/.rtV2U,u]//FullSimplify;

coeff2lp=Quiet[Solve[Flatten[%]==0,{lm,l0,(*lp,*)Lm,L0,Lp},Reals]]//Simplify//First

%%%//.coeff2lp//Simplify


(* ::Input::Initialization:: *)
lpRule=lp->-1/(SubMinus[u]-SubPlus[u]);

coeffs=coeffNames/.coeff2lp/.lpRule//Simplify;
coeffRules=Thread[coeffNames->coeffs]


(* ::Input::Initialization:: *)
\[Xi]=\[Chi]/.coeffRules//FullSimplify


(* ::Input:: *)
(*(\[Xi]/.coeffRules/.rtZ2uv/.rtV2U)//FullSimplify*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Bulk modular flow generator*)


(* ::Input:: *)
(*Framed[\[Chi]def]*)


(* ::Input:: *)
(*Framed[coeffRules]*)


(* ::Input:: *)
(*Framed[\[Xi]]*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*Check surface gravity:*)


(* ::Text:: *)
(*T=\[Kappa]/(2\[Pi])=1, \[Kappa]^2=(2\[Pi])^2=-(1/2)(\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SubscriptBox[\(\[Xi]\), \(\[Nu]\)]\))(\[Del]^\[Mu]\[Xi]^\[Nu])*)


(* ::Input:: *)
(*Simplify[*)
(*contract[*)
(*covD[lower[\[Xi]]]**covD[lower[\[Xi]]],*)
(*{1,3},{2,4}*)
(*]/.rtZ2uv/.rtZuv/.rtV2U]*)


(* ::Input:: *)
(**)


(* ::Section:: *)
(*Flow lines*)


(* ::Input::Initialization:: *)
flow={t[s],x[s],z[s]};
dflow=\!\(
\*SubscriptBox[\(\[PartialD]\), \(s\)]flow\)


(* ::Subsubsection::Closed:: *)
(*Numerical results*)


(* ::Text:: *)
(*(t,x,z): coordinates*)


(* ::Input:: *)
(* {1/2 \[Xi].{1,-1,0},1/2 \[Xi].{1,1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->1,SubPlus[v]->1,SubMinus[u]->-1,SubMinus[v]->-1}//Simplify*)
(**)
(*Thread[dflow==(%/.Thread[{t,x,z}->flow])]*)
(*Thread[(flow/.s->0)=={0,xc,zc}]*)
(*%%~Join~%;*)
(**)
(*flowLine[xc_,zc_]=Quiet[NDSolve[%//Evaluate,flow//Evaluate,{s,-20,20}]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Module[{halfL=1,zc=.35,smax=10,valuesX,valuesS,pts},*)
(*valuesX=Subdivide[-.99halfL,.99halfL,50];*)
(*valuesS=PowerRange[.001,smax,1.2]*#&/@{-1,1}//Flatten;*)
(**)
(*"# NOTE: Errors are silenced";*)
(*pts=Quiet[(flow/.First[flowLine[#,zc]])&/@valuesX];*)
(*pts=Quiet[(pts/.s->#)&/@valuesS];*)
(*pts=( *)
(*pts//Flatten[#,{1,2}]&//Select[And[-1<#[[1]]<1,-1<#[[2]]<1,0<#[[3]]<zc]&]*)
(*);*)
(**)
(*Show[*)
(*ListPlot3D[pts,*)
(*PlotRange->{{-1,1},{-1,1},{0,1}},*)
(*InterpolationOrder->4*)
(*,AxesLabel->{t,x,z}*)
(*,ViewPoint->{.5,1.35,.45}*)
(*,ViewProjection->"Orthographic"*)
(*,BoxRatios->Automatic*)
(*,ColorFunction->"SouthwestColors"*)
(*,PlotStyle->Directive[Opacity[.8]]*)
(*,MeshStyle->Directive[Gray]*)
(*,Mesh->10*)
(*],*)
(**)
(*ParametricPlot3D[{0,x,zc},{x,-Sqrt[1-zc^2],Sqrt[1-zc^2]},Mesh->None,PlotStyle->Directive[RGBColor[0.12582006271512805`, 0.5293439498278976, 0.7809840752581536],Thickness[.0135]]],*)
(*ParametricPlot3D[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],0,\[Pi]},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.]]],*)
(*ParametricPlot3D[{0,r Cos[\[Theta]],r Sin[\[Theta]]},{\[Theta],0,\[Pi]},{r,0,1},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.],Opacity[.2]]],Graphics3D[{Gray,Thickness[0.005],Opacity[.03],Polygon[{{-1,0,0},{0,1,0},{1,0,0},{0,-1,0}}]}],*)
(**)
(*ImageSize->UpTo[480]*)
(**)
(*]*)
(*]*)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Analytic results*)


(* ::Text:: *)
(*One can actually solve the flow equations by hand: note that \[DifferentialD]x/\[DifferentialD]z=x/z, which means x/Subscript[x, c]=z/Subscript[z, c],*)


(* ::Item:: *)
(*L below should be understood as Subscript[L, \[Infinity]]*)


(* ::Input::Initialization:: *)
 {1/2 \[Xi].{1,-1,0},1/2 \[Xi].{1,1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->L/2,SubPlus[v]->L/2,SubMinus[u]->-(L/2),SubMinus[v]->-(L/2)}//Simplify;

Thread[dflow==(%/.Thread[{t,x,z}->flow])];
Thread[(flow/.s->0)=={0,xc,zc}];
%%~Join~%;

flowRules=%/.(Equal->Rule);

x'[s]/z'[s]/.%

z'[t]==z'[s]/t'[s]/.%%/.x[t_]:>xc/zc z[t]/.{t[s]->t,z[s]->z[t]}//Simplify;
%/.Solve[1+xc^2/zc^2==a,xc][[1]]//Simplify

Assuming[0<zc<1&&0<a<L^2/zc^2,
DSolve[{%,z[0]==zc},z[t],t]//FullSimplify;
]
Assuming[0<zc<L/2&&0<a<(L/2)^2/zc^2,
z[t]/.%/.t->0//FullSimplify
]

%%[[1]]//FullSimplify
zFlow[zc_,xc_,t_]=z[t]/.Assuming[0<zc<1&&0<a<1/zc^2,
%/.a->1+xc^2/zc^2//FullSimplify
];

%%/.L->2R//Simplify

"tried to solve for flow parameter, but failed";
(*%%%/.t\[Rule]t[s]/.z[t[s]]\[Rule]z[s];*)
(*(t'[s]/.flowRules/.x[t_]\[RuleDelayed]xc/zcz[t]//Simplify)/.Solve[1+xc^2/zc^2\[Equal]a,xc]\[LeftDoubleBracket]1\[RightDoubleBracket]/.%//Simplify*)
(*DSolve[{t'[s]\[Equal]%,t[0]\[Equal]0},t[s],s]*)


(* ::Input:: *)
(*modFlowAnalytic=Module[{halfL=1,zc=.4,smax=10,valuesX,valuesS,pts},*)
(*valuesX=Subdivide[-.99halfL,.99halfL,50];*)
(*valuesS=PowerRange[.001,smax,1.2]*#&/@{-1,1}//Flatten;*)
(**)
(*Show[*)
(*ParametricPlot3D[*)
(*(* {1/2\[Xi].{1,-1,0},1/2\[Xi].{1,1,0},\[Xi]\[LeftDoubleBracket]-1\[RightDoubleBracket]}/.\[VeryThinSpace]{u\[Rule]x+t,v\[Rule]x-t,Subscript[u, +]\[Rule]1,Subscript[v, +]\[Rule]1,Subscript[u, -]\[Rule]-1,Subscript[v, -]\[Rule]-1}//Simplify//Evaluate,*)*)
(*{t,xc/zc zFlow[zc,xc,t],zFlow[zc,xc,t]},*)
(**)
(*{t,-1,1},*)
(*{xc,-Sqrt[halfL^2-zc^2],Sqrt[halfL^2-zc^2]},*)
(**)
(*PlotRange->{{-1,1},{-1,1},{0,1}}*)
(*,PlotPoints->100*)
(*,AxesLabel->{HoldForm[t],HoldForm[x],HoldForm[z]}*)
(*,ViewPoint->{.5,1.35,.45}*)
(*,ViewProjection->"Orthographic"*)
(*,BoxRatios->Automatic*)
(*,ColorFunction->"SouthwestColors"*)
(*,PlotStyle->Directive[Opacity[.8]]*)
(*,MeshStyle->Directive[Gray]*)
(*,Mesh->10*)
(*],*)
(**)
(*ParametricPlot3D[{0,x,zc},{x,-Sqrt[1-zc^2],Sqrt[1-zc^2]},Mesh->None,PlotStyle->Directive[RGBColor[0.12582006271512805`, 0.5293439498278976, 0.7809840752581536],Thickness[.0135]]],*)
(*ParametricPlot3D[{0,Cos[\[Theta]],Sin[\[Theta]]},{\[Theta],0,\[Pi]},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.]]],*)
(*ParametricPlot3D[{0,r Cos[\[Theta]],r Sin[\[Theta]]},{\[Theta],0,\[Pi]},{r,0,1},Mesh->None,PlotStyle->Directive[RGBColor[0.790588, 0.201176, 0.],Opacity[.2]]],Graphics3D[{Gray,Thickness[0.005],Opacity[.03],Polygon[{{-1,0,0},{0,1,0},{1,0,0},{0,-1,0}}]}],*)
(**)
(*ImageSize->UpTo[480]*)
(**)
(*]*)
(*]*)
(**)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Modular flow generator at the cutoff surface*)


(* ::Text:: *)
(*Rapidity \[Theta] parametrize the boost of the interval:*)


(* ::Input:: *)
(*$Assumptions=L>0;*)
(**)
(*FullSimplify[\[Xi]/.{SubMinus[u]->-SubPlus[u],SubMinus[v]->-SubPlus[v]}/.{SubPlus[u]->SubPlus[x]+SubPlus[t],SubPlus[v]->SubPlus[x]-SubPlus[t]}]//Together//Collect[#,SubPlus[x]]&//ExpandDenominator*)
(**)
(*%//.{SubPlus[x]->L/2 Cosh[\[Theta]],SubPlus[t]->L/2 Sinh[\[Theta]]}//Simplify*)
(**)
(*((Hold[({*)
(* {\!\( *)
(*  \*SubscriptBox[\(\[PartialD]\), \(u\)]t\), \!\( *)
(*  \*SubscriptBox[\(\[PartialD]\), \(v\)]t\), 0},*)
(* {\!\( *)
(*  \*SubscriptBox[\(\[PartialD]\), \(u\)]x\), \!\( *)
(*  \*SubscriptBox[\(\[PartialD]\), \(v\)]x\), 0},*)
(* {0, 0, 1}*)
(*})]/.xt2uv//ReleaseHold).%)/.uv2xt//Simplify*)
(**)
(*collectCoefficient[%,(2\[Pi])/L,FullSimplify]//Framed*)
(**)


(* ::Input:: *)
(**)


(* ::Subsubsection:: *)
(*Rindler Map / CHM Map*)


(* ::Text:: *)
(*Map the casual diamond to the BTZ coordinates:*)


(* ::Input::Initialization:: *)
poincareXT`metricForm=#.metric.#&[Dt/@coord]/.{u->x+t,v->x-t}//Simplify

%/.{z->1/Sqrt[2\[Rho]]}//Expand//Simplify

diamond2btzForm=HoldForm[{
t->L/2 (((r-Tu Tv)Sinh[Tu u-Tv v])/(Sqrt[r^2-(Tu Tv)^2]Cosh[Tu u+Tv v]+(r-Tu Tv)Cosh[Tu u-Tv v])),
x->L/2 ((Sqrt[r^2-(Tu Tv)^2]Sinh[Tu u+Tv v])/(Sqrt[r^2-(Tu Tv)^2]Cosh[Tu u+Tv v]+(r-Tu Tv)Cosh[Tu u-Tv v])),
z->L/2 ((Sqrt[2Tu Tv] Sqrt[r-Tu Tv])/(Sqrt[r^2-(Tu Tv)^2]Cosh[Tu u+Tv v]+(r-Tu Tv)Cosh[Tu u-Tv v]))
}];

diamond2btz=ReleaseHold[diamond2btzForm];


(* ::Input:: *)
(*poincareXT`metricForm/.diamond2btz/.{Dt[L]->0};*)
(**)
(*Outer[Times,#,#]&@(Dt/@{u,v,r})//Flatten//Union;*)
(**)
(*Coefficient[%%,#]&/@%//FullSimplify;*)
(**)
(*{%%,%}//Grid[#,Frame->All]&*)
(**)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Constant z line now looks like a Bell curve:*)


(* ::Input:: *)
(*Plot[z/.diamond2btz/.r->10./.u->.1/.L->1//Simplify,{v,-10,10}]*)


(* ::Item:: *)
(*Rindler horizon at finite u,v gets mapped to infinity:*)


(* ::Input:: *)
(*Assuming[r>1,*)
(*Series[#/.diamond2btz//Simplify//Evaluate,{r,\[Infinity],0}]&/@{x+t,x-t}//Simplify*)
(*]*)


(* ::Input:: *)
(*Plot[ArcTanh[u],{u,-5,5}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*diamond2btzForm//toLaTeX\*)
(*//StringReplace["\\left("->"\\pqty{"]\*)
(*//StringReplace["\\right)"->"}"]*)


(* ::Input:: *)
(**)


(* ::Item:: *)
(*Modular flow now reduces to simply Subscript[\[PartialD], \[Tau]]*)


(* ::Input:: *)
(*({1/2 \[Xi].{1,-1,0},1/2 \[Xi].{1,1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->L/2,SubPlus[v]->L/2,SubMinus[u]->-(L/2),SubMinus[v]->-(L/2)}//Simplify)/.diamond2btz//Simplify*)


(* ::Input:: *)
(*<<"Physica/GRUtils.wl";*)
(*jacobianBTZbyPoincare=jacobianFromFunc[*)
(*Function[{u,v,r},{t,x,z}/.diamond2btz//Evaluate],*)
(*{u,v,r}*)
(*]//Inverse;*)
(**)
(*jacobianBTZbyPoincare.({1/2 \[Xi].{1,-1,0},1/2 \[Xi].{1,1,0},\[Xi][[-1]]}/. {u->x+t,v->x-t,SubPlus[u]->L/2,SubPlus[v]->L/2,SubMinus[u]->-(L/2),SubMinus[v]->-(L/2)}/.diamond2btz)//Simplify*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\(( *)
(*\*FractionBox[\(u - v\), \(2\)])\)\)-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]\(( *)
(*\*FractionBox[\(u - v\), \(2\)])\)\)*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
eqZr=(zc==z)/.diamond2btz/.{Tu->T,Tv->T}/.uv2xt/.t->0//Simplify


(* ::Input::Initialization:: *)
r2s=Solve[Sqrt[r-T^2]==T/Sqrt[2] s,r]//Flatten//Simplify

eqZr/.r2s//Simplify[#,{s>0}]&
Solve[%,s]//Simplify//Flatten

s2xBTZ=%[[2]]


(* ::Input::Initialization:: *)
r/.r2s/.s2xBTZ//Simplify

rCutoff=Function[{x,zc,L,T},%//Evaluate];


(* ::Input::Initialization:: *)
s==0/.s2xBTZ//Simplify
%[[1,1]]==0//Simplify
#^2&/@%

Cosh[4T x]==2Cosh[2T x]^2-1//Simplify
%%/.(Cosh[4T x]->2Cosh[2T x]^2-1)/.Cosh[2T x]->w

Solve[%,w]

Solve[Cosh[2T x]==w/.%[[-1]],x,Reals]//Simplify[#,{0<zc<L/2}]&//Last;
xcBTZ=Function[{zc,L,T},x/.%//Evaluate]


(* ::Input:: *)
(*(Csch[x]^2==1/(Cosh[x]^2-1))//Simplify*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(s/.s2xBTZ//Simplify)/.(Cosh[4T x]->2Cosh[2T x]^2-1)/.Csch[2T x]^2->1/(Cosh[2T x]^2-1)/.Cosh[2T x]->w//FullSimplify*)
(*w^2-1/.w->Cosh[2T x]//Simplify*)
(**)
(*%%/.-1+w^2->Sinh[2 T x]^2/.w->Cosh[2T x]//Simplify[#,{Cosh[2T x]>1}]&*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*rCutoff*)


(* ::Input:: *)
(*(L^2+2 zc^2-2 zc^2 Cosh[4 T x]/.(Cosh[4T x]->2Cosh[2T x]^2-1)/.Cosh[2T x]->w//Simplify)/.-1+w^2->Sinh[2 T x]^2*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*Module[{halfL=1,T=1/2,zcRange,L},*)
(*L=2halfL;*)
(*zcRange=Range[.1,halfL-.1,.1];*)
(*Show[*)
(*LogPlot[*)
(*rCutoff[x,#,L,T]UnitBox[x/2/xcBTZ[#,L,T]]&/@zcRange//Evaluate,*)
(*{x,-5,5},PlotRange->{{-5,5},{.1,25}}*)
(*,AspectRatio->Automatic*)
(*,AxesLabel->{\[Phi],\[Rho]}*)
(*],*)
(*LogPlot[*)
(*T^2,{x,-4,4}*)
(*,PlotStyle->{Dashed,colors[[5]]}*)
(*,PlotLabels->Placed[HoldForm["\!\(\*SuperscriptBox[\(T\), \(2\)]\)"],After]*)
(*]*)
(*]*)
(*]*)


(* ::Input:: *)
(**)
(**)
(**)


(* ::Item:: *)
(*TO CHECK: Boundary map:*)


(* ::Item:: *)
(*Again L below should be understood as Subscript[L, \[Infinity]]*)


(* ::Input::Initialization:: *)
{t,x,z}/.diamond2btz//Series[#,r->\[Infinity]]&

Thread[{t,x,z}->%][[;;2]]//Normal//Simplify
diamond2btz2D=%/.L->Sqrt[L^2-4 zc^2]/.Tv->Tu/.Tu->1/2 Sqrt[L^2-(2zc)^2]/L//Simplify

-Dt[t]^2+Dt[x]^2/.%/.{Dt[L]->0,Dt[Tu]->0,Dt[Tv]->0}/.{Dt[L]->0,Dt[zc]->0}//Simplify


(* ::Item:: *)
(*Modular flow now reduces to simply Subscript[\[PartialD], \[Tau]]*)


(* ::Input::Initialization:: *)
\[Xi]cutoff=(2\[Pi])/L {(L/2)^2-(t^2+x^2+zc^2),-2 t x};

\[Xi]cutoff/.diamond2btz2D//Simplify


(* ::Input:: *)
(*<<"Physica/GRUtils.wl";*)
(*jacobianBTZbyPoincare2D=jacobianFromFunc[*)
(*Function[{u,v},{t,x}/.diamond2btz2D//Evaluate],*)
(*{u,v}*)
(*]//Inverse;*)
(**)
(*jacobianBTZbyPoincare2D.\[Xi]cutoff/.diamond2btz2D(*/.Tu\[Rule]Tv/.Tv\[Rule]1/2*)//Simplify*)


(* ::Input:: *)
(*\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(u\)]\(( *)
(*\*FractionBox[\(u - v\), \(2\)])\)\)-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(v\)]\(( *)
(*\*FractionBox[\(u - v\), \(2\)])\)\)*)


(* ::Input:: *)
(*{x,t}/.diamond2btz2D/.uv2xt/.{L->3,zc->.4}//Evaluate*)


(* ::Input:: *)
(*ParametricPlot[( *)
(*{x,t}/.diamond2btz2D/.uv2xt/.{L->2,zc->.0001}/.x->#*)
(*)&/@Subdivide[-5,5,20]//Evaluate,*)
(*{t,-10,10}*)
(*,PlotRange->All*)
(*,AspectRatio->Automatic*)
(*]*)


(* ::Input:: *)
(**)
(**)


(* ::Input:: *)
(*(*%/.{*)
(*t\[Rule]L/2(Sin[\[Theta]]Sinh[2\[Tau]/L])/(1+Sin[\[Theta]]Cosh[2\[Tau]/L]),*)
(*x\[Rule]L/2Cos[\[Theta]]/(1+Sin[\[Theta]]Cosh[2\[Tau]/L])*)
(*}//Simplify*)*)


(* ::Input:: *)
(**)


(* ::Input::Initialization:: *)
saveScript[];
