package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.model.phase.PhaseListModel;
import com.molkky.molkky.model.phase.PhaseModel;
import com.molkky.molkky.repository.PhaseRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.PhaseService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpSession;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/phase")
public class PhaseController {

    @Autowired
    PhaseService phaseService;

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    PhaseRepository phaseRepository;

    @GetMapping("/{id}/generate")
    public void generate(@PathVariable String id){
        phaseService.generate(id);
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
    public String savePhases(@ModelAttribute("form")PhaseListModel phasesModel, ModelMap model) throws ParseException {
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
        return "redirect:/tournament/view?tournamentId="+t.getId();
    }
}
