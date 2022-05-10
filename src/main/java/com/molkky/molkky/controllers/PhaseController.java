package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.PhaseListModel;
import com.molkky.molkky.model.PhaseModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.PhaseService;
import lombok.Getter;
import org.apache.xpath.operations.Mod;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import type.PhaseType;

import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/phase")
public class PhaseController {

    @Autowired
    PhaseService phaseService;

    @Autowired
    TournamentRepository tournamentRepository;

    @GetMapping("/{id}/generate")
    public void generate(@PathVariable String id){
        phaseService.generate(id);

    }

    @GetMapping("/choosePhase")
    public String choosePhase(Model model, Integer tournamentId){
        Tournament t = tournamentRepository.findById(1);
        PhaseListModel phases = new PhaseListModel();
        for(int i = 0;i< t.getNbRounds();i++){
            phases.add(new PhaseModel());
        }
        model.addAttribute("form",phases);
        return "/phase/choosePhase";
    }


}
