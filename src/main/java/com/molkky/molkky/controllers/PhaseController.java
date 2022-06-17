package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Match;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Round;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.phase.PhaseListModel;
import com.molkky.molkky.model.phase.PhaseModel;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.RoundRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.PhaseService;
import com.molkky.molkky.service.RoundService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import type.UserRole;

import javax.servlet.http.HttpSession;
import java.text.ParseException;
import java.util.*;

@Controller
@RequestMapping("/phase")
public class PhaseController extends DefaultAttributes {

    @Autowired
    PhaseService phaseService;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @Autowired
    RoundRepository roundRepository;

    @Autowired
    RoundService roundService;

    private String tournamentView = "redirect:/tournament/view?tournamentId=";
    private String phaseView = "redirect:/phase/view?id=";

    @PostMapping("/generate")
    public String generate(Model model, HttpSession session, @RequestParam(name = "id", required = true) String id){

        UserLogged user = (UserLogged) session.getAttribute("user");

        if(user == null){
            return "redirect:/connexion";
        }

        if(user.getRole().equals(UserRole.ADM) ){
            Phase phase = phaseRepository.findById(Integer.valueOf(id));

            if(Boolean.TRUE.equals(phase.getFinished())) return tournamentView+phase.getTournament().getId();

            Tournament tournament = phase.getTournament();
            tournament.setIndexPhase(tournament.getIndexPhase()+1);
            tournamentRepository.save(tournament);

            Map<Round, List<Match>> response = phaseService.generate(id);

            model.addAttribute("round_match", response);
        }else{
            return "redirect:/";
        }
        return phaseView+id;
    }

    @PostMapping("/round")
    public String nextRound(Model model, HttpSession session, @RequestParam(name = "id", required = true) String id,  @RequestParam(name = "nbSet", required = false) String nbSet){
        UserLogged user = (UserLogged) session.getAttribute("user");

        if(user == null){
            return "redirect:/connexion";
        }

        Phase phase = phaseRepository.findById(Integer.valueOf(id));

        if(Boolean.TRUE.equals(phase.getFinished())) return tournamentView+phase.getTournament().getId();

        phase.setNbSets(Integer.valueOf(nbSet));
        phaseRepository.save(phase);


        if(user.getRole().equals(UserRole.ADM) ){
            Map<Round, List<Match>> response = phaseService.generate(id);

            model.addAttribute("round_match", response);
        }else{
            return "redirect:/";
        }
        return phaseView+id;

    }

    @GetMapping("/choosePhases")
    public String choosePhase(Model model, @RequestParam(value = "tournamentId") String tournamentId, HttpSession session){
        UserLogged user = (UserLogged)session.getAttribute("user");
        Tournament t = tournamentRepository.findById(Integer.valueOf(tournamentId));
        PhaseListModel phases = new PhaseListModel();
        for(int i = 0;i< t.getNbRounds();i++){
            PhaseModel phase = new PhaseModel();
            phase.setTournament(t.getId());
            phase.setNbCourts(t.getNbCourts());
            phases.add(phase);
        }
        model.addAttribute("user", user);
        model.addAttribute("form",phases);
        return "/phase/choosePhases";
    }

    @PostMapping("/choosePhases")
    public ModelAndView sendPhase(@ModelAttribute("form")PhaseListModel phases, ModelMap model){
        Integer nbPhase = 1;
        for(PhaseModel phase : phases.getPhases()){
            phase.setNbPhase(nbPhase);

            nbPhase++;
        }
        model.addAttribute("listPhase",phases);
        return new ModelAndView("/phase/editPhases",model);
    }

    @PostMapping("/editPhases")
    public String savePhases(@ModelAttribute("listPhase")PhaseListModel phasesModel, ModelMap model) throws ParseException {
        Tournament t = tournamentRepository.findById(phasesModel.getPhases().get(0).getTournament());
        List<Phase> phases = new ArrayList<>();
        for(PhaseModel phase : phasesModel.getPhases()){
            switch (phase.getPhaseType()){
                case POOL:
                    phases.add(new Pool(phase,t));
                    break;
                case FINNISH:
                    phases.add(new Finnish(phase,t));
                    break;
                case KNOCKOUT:
                    phases.add(new Knockout(phase,t));
                    break;
                case SWISSPOOL:
                    phases.add(new SwissPool(phase,t));
                    break;
                case SIMPLEGAME:
                    phases.add(new SimpleGame(phase,t));
                    break;
                default:
                    break;
            }
        }
        phaseRepository.saveAll(phases);
        t.setPhases(phases);
        t =  tournamentRepository.save(t);
        return tournamentView+t.getId();
    }

    @GetMapping("/modify")
    public ModelAndView modifyPhase(ModelMap model, @RequestParam(value = "tournamentId") String tournamentId){
        Tournament t = tournamentRepository.findById(Integer.valueOf(tournamentId));
        List<Phase> phaseList = t.getPhases();
        PhaseListModel phaseListModel = new PhaseListModel();
        for(Phase phase : phaseList){
            phaseListModel.add(new PhaseModel(phase));
        }
        model.addAttribute("listPhase",phaseListModel);
        return new ModelAndView("/phase/modifyPhase",model);
    }

    @PostMapping("/modify")
    public String changeInfoPhases(@ModelAttribute("listPhase")PhaseListModel phasesModel, ModelMap model){
        Tournament t = phaseService.editPhasesInfo(phasesModel);
        return tournamentView+t.getId();
    }

    @PostMapping("/view")
    public String viewPost( @RequestParam(name= "id") Integer id,  @RequestParam(name= "phaseIndex") Integer phaseIndex ){
        return phaseView+id + "&phaseIndex=" + phaseIndex;
    }

    @GetMapping("/view")
    public String view(Model model, HttpSession session, @RequestParam(name= "id") Integer id,
                       @RequestParam(name= "phaseIndex") Integer phaseIndex){

        Phase phase = phaseRepository.findById(id);
        List<Round> rounds = phase.getRounds();
        Collections.reverse(rounds);
        Map<Round,  List<PhaseRankingModel>> roundTeams = new LinkedHashMap<>();
            for (Round r : rounds) {
                List<PhaseRankingModel> teams = roundService.orderTeamsByScoreInRound(r, phase.getVictoryValue());
                roundTeams.put(r, teams);
            }

            if(phase instanceof SwissPool){
                List<PhaseRankingModel> teams = roundService.orderTeamsByScoreInPhase(phase, phase.getVictoryValue());
                model.addAttribute("phaseTotal", teams);
            }else{
                model.addAttribute("phaseTotal", new ArrayList<>());
            }

        model.addAttribute("rounds", rounds);
        model.addAttribute("roundTeams", roundTeams);
        model.addAttribute("phaseIndex", phaseIndex);

        model.addAttribute("currentPhase", phase);
        model.addAttribute("currentTournament", phase.getTournament());


        return "/phase/view";
    }

}
