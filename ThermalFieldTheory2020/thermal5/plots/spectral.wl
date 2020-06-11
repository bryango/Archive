#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Input:: *)
(*(*wipeAll[]*)*)


(* ::Input::Initialization:: *)
SetDirectory@NotebookDirectory[];
<<"MathUtils.wl"
(*<<MaTeX`
SetOptions[MaTeX,"Preamble"\[Rule]{"\\usepackage{physics,siunitx}"}];*)


(* ::Input:: *)
(*q=(2w-k^2)/(2k);*)
(*e=q^2/2;*)
(*rho[w_,k_,t_,u_]=(Sign[q]t)/k Log[(1+E^(-(e-u)/t))/(1+E^(-(e-u+w)/t))];*)
(*Manipulate[*)
(*Quiet[*)
(*Plot[rho[w,k,t,u],{w,0,5},PlotRange->All,Exclusions->None]*)
(*]*)
(*,{{{t,1},.01,10,#},{{u,1},-5,5,#},{{k,1},0,20,#}}&[*)
(*Appearance->"Open"*)
(*]//Apply[Sequence]//Evaluate*)
(*]*)


(* ::Input:: *)
(*spectralmu=Plot[{*)
(*rho[w,1,1,-1],*)
(*rho[w,1,1,0],*)
(*rho[w,1,1,1]*)
(*},{w,0,4}*)
(*,PlotRange->All,Exclusions->None*)
(*,PlotLegends->Evaluate[Style[#,12]&/@{*)
(*"\[Mu] = 1",*)
(*"\[Mu] = 0",*)
(*"\[Mu] = -1"*)
(*}]*)
(*,AxesLabel->{\[Omega],\[Rho]}*)
(*,PlotLabel->"k = 1"*)
(*]*)
(*exportPlot[spectralmu,"./"]*)


(* ::Input:: *)
(*spectralk=Plot[{*)
(*rho[w,1,1,0],*)
(*rho[w,2,1,0],*)
(*rho[w,3,1,0]*)
(*},{w,0,8}*)
(*,PlotRange->All,Exclusions->None*)
(*,PlotLegends->Evaluate[Style[#,12]&/@{*)
(*"k = 1",*)
(*"k = 2",*)
(*"k = 3"*)
(*}]*)
(*,AxesLabel->{\[Omega],\[Rho]}*)
(*,PlotLabel->"\[Mu] = 0"*)
(*]*)
(*exportPlot[spectralk,"./"]*)


(* ::Input:: *)
(*saveScript[]*)
