package com.molkky.molkky.controllers;


import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.TournamentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import type.TournamentStatus;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
@RequestMapping("/tournament")
public class TournamentController extends DefaultAttributes {
    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private TournamentService tournamentService;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/create")
    public String tournamentForm(Model model, HttpSession session) {
        model.addAttribute("tournament", new TournamentModel());
        return "/tournament/create";
    }

    @PostMapping("/create")
    public String tournamentSubmit(@Valid @ModelAttribute("tournament") TournamentModel tournament, Model model) {

        Tournament tournamentEntity = tournamentService.create(tournament);

        int id = tournamentEntity.getId();
        return "redirect:/tournament/"+id+"/view";
    }

    @GetMapping("/{id}/view")
    public String tournamentView(Model model, @PathVariable("id") String id){
        Tournament tournament = tournamentRepository.findById(Integer.valueOf(id));
        model.addAttribute("tournament", tournament);
        model.addAttribute("nbTeam", tournament.getTeams().size());

        return "/tournament/view";
    }



    @PostMapping(value = "/view" , params = "launch")
    public String tournamentViewPostLaunch(@RequestParam(value = "tournamentId", required = false) String tournamentId){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setStatus(TournamentStatus.INPROGRESS);

        tournamentRepository.save(tournament);

        return "redirect:/tournament/create";
    }



}