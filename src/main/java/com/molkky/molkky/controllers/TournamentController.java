package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import type.TournamentStatus;

@Controller
@RequestMapping("/tournament")
public class TournamentController {
    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/create")
    public String tournamentForm(Model model) {
        model.addAttribute("tournament", new TournamentModel());
        return "tournament/create";
    }

    @PostMapping("/create")
    public String tournamentSubmit(@ModelAttribute("tournament") TournamentModel tournament, Model model) {

        Tournament tournamentEntity = tournamentRepository.save(new Tournament(tournament));
        
        model.addAttribute("tournament", tournamentEntity);
        return "tournament/create";
    }

    @GetMapping("/{id}/view")
    public String tournamentView(Model model, @PathVariable("id") String id){

        //USER FROM SESSION
        User user = userRepository.findById(4);

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(id));
        model.addAttribute("tournament", tournament);
        //model.addAttribute("tournamentId", "0");
        model.addAttribute("user", user);
        model.addAttribute("nbTeam", tournament.getTeams().size());

        return "tournament/view";
    }



    @PostMapping(value = "/view" , params = "launch")
    public String tournamentViewPostLaunch(@RequestParam(value = "tournamentId", required = false) String tournamentId){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setStatus(TournamentStatus.INPROGRESS);

        tournamentRepository.save(tournament);

        return "redirect:/tournament/create";
    }



}