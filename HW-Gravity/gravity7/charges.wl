#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input:: *)
(*(*wipeAll[];*)*)


(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[]


(* ::Section:: *)
(*Package Notes*)


(* ::Item:: *)
(*We use `diffgeo` to compute the exterior derivatives, and to confirm our final result.*)


(* ::Subitem:: *)
(*`diffgeoM` is a customized version of `diffgeo` and can be obtained from:*)


(* ::Subsubitem:: *)
(*https://github.com/bryango/diffgeoM*)


(* ::Item:: *)
(*Some other helper scripts can be obtained from:*)


(* ::Subsubitem:: *)
(*https://github.com/bryango/Physica*)


(* ::Subitem:: *)
(*MathUtils.wl, a helper script for personal convenience.*)


(* ::Subitem:: *)
(*GRUtils.wl, a helper script for some GR calculations, e.g. Jacobians.*)


(* ::Section:: *)
(*Setup*)


(* ::Input::Initialization:: *)
<<"Physica/MathUtils.wl";
<<"Physica/GRUtils.wl";


(* ::Input::Initialization:: *)
"# for diffgeoM, det g = -1";
metricSign=-1;


(* ::Input::Initialization:: *)
constants={gN,m};
SetAttributes[#,Constant]&/@constants;
$Assumptions=And@@(Greater[#,0]&/@constants)


(* ::Input::Initialization:: *)
coord={t,r,\[Theta],\[Phi]};
addAssumption[r>0];
addAssumption[rc>0]


(* ::Item:: *)
(*This is the redshift squared f(x):*)


(* ::Input::Initialization:: *)
f=Function[r,1-(2gN*m)/r];


(* ::Input::Initialization:: *)
metricForm=Function[coord//Evaluate,
-f[r] Dt[t]^2+Dt[r]^2/f[r]+r^2 (Dt[\[Theta]]^2+Sin[\[Theta]]^2 Dt[\[Phi]]^2)
];

metric=quadToMatrix[
metricForm@@coord,
coord
];


(* ::Input::Initialization:: *)
<<diffgeoM`
tRicci//MatrixForm


(* ::Item:: *)
(*We see that the Ricci curvature is zero (outside the horizon), as expected.*)


(* ::Section:: *)
(*Charge*)


(* ::Item:: *)
(*Here we specify the parameter to vary: m*)


(* ::Input::Initialization:: *)
varParams={m}//holdItems//Map[ToString];
Attributes/@varParams

varAssoc=AssociationMap["d"<>#&,varParams]\
//KeyMap[ToExpression]//ToExpression;

varDiffs=Values[varAssoc]


(* ::Item:: *)
(* Metric variation is given by \[Delta]g:*)


(* ::Input::Initialization:: *)
\[Delta]g=Total[
KeyValueMap[
D[g,#1]*#2&,
varAssoc
]
];

printPrevious[MatrixForm@*Simplify]


(* ::Item::Initialization:: *)
(*Now we define the anti-symmetrized two form Subscript[k, \[Xi]]^\[Mu]\[Nu]; the input \[Xi]=xi is assumed to be a co-vector Subscript[\[Xi], \[Mu]].*)


(* ::Input::Initialization:: *)
"Note that \[Del] is metric compatible";
k[xi_,dg_]:=(-1)(
+contract[xi**\[Del]dg,{3,4}]
-contract[xi**\[Del]dg,{2,3}]
+contract[xi**\[Del]dg,{1,3}]
+1/2 contract[dg**\[Del]xi,{1,2}]
-1/2 contract[dg**\[Del]xi,{2,3}]
+1/2 contract[dg**\[Del]xi,{2,4}]
)//raise//antisymmetrize;


(* ::Item:: *)
(*The variation of the energy corresponds to the Killing vector \!\(\[Xi] = *)
(*\*SubscriptBox[\(\[PartialD]\), \(t\)]\). *)


(* ::Input::Initialization:: *)
\[Xi]={1,0,0,0};
lieD[\[Xi],g]

k0=k[lower[\[Xi]],\[Delta]g]//Simplify


(* ::Item:: *)
(*The charge variation is given by the integral of  Subscript[k, \[Xi]]^\[Mu]\[Nu] along the S^2:*)


(* ::Input::Initialization:: *)
\[Delta]E=(2\[Pi])/(8\[Pi] gN) Integrate[Evaluate[vol*k0[[t,r]]]/.r->rc,{\[Theta],0,\[Pi]}]


(* ::Section:: *)
(*Surface gravity*)


(* ::Item:: *)
(*Surface gravity \[Kappa] by definition is given by \[Xi]^\[Lambda] \!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Lambda]\)]*)
(*\*SuperscriptBox[\(\[Xi]\), \(\[Mu]\)]\)=\[Kappa] \[Xi]^\[Mu]*)


(* ::Input::Initialization:: *)
contract[lower[\[Xi]]**covD[\[Xi],{up}],{1,2}]//Simplify


(* ::Item:: *)
(*Alternatively, the LHS can be written as -(1/2)\[Del]^\[Mu](Subscript[\[Xi], \[Lambda]] \[Xi]^\[Lambda]) or -Subscript[\[Xi], \[Lambda]]\[Del]^\[Mu]\[Xi]^\[Lambda]*)


(* ::Input:: *)
(*norm[lower[\[Xi]]]//Simplify*)
(*-1/2 raise[pD[%]]//Simplify*)
(**)
(*contract[*)
(*-lower[\[Xi]]**raise[covD[lower[\[Xi]]],{1}],*)
(*{1,3}]//Simplify*)


(* ::Item:: *)
(*The equation is of the form 0=0 at the horizon, so it's hard to determine \[Kappa].*)


(* ::Item:: *)
(*Alternatively, try \[Kappa]^2=-(1/2)(\[Del]^\[Mu]\[Xi]^\[Lambda])(\!\( *)
(*\*SubscriptBox[\(\[Del]\), \(\[Mu]\)]*)
(*\*SubscriptBox[\(\[Xi]\), \(\[Lambda]\)]\)):*)


(* ::Input::Initialization:: *)
covD[lower[\[Xi]]]//Simplify
Sqrt[-(1/2)norm[%]]//Simplify
%/.r->2gN m


(* ::Item:: *)
(*This gives the Schwarzschild surface gravity as expected*)


(* ::Item:: *)
(*References:*)


(* ::Subitem:: *)
(*Wald, Section 12.5*)


(* ::Subitem:: *)
(*https://physics.stackexchange.com/q/403281*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*saveScript[]*)
