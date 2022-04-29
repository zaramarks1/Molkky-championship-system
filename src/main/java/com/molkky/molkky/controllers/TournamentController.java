package com.molkky.molkky.controllers;

import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class TournamentController {
    @Autowired
    private TournamentRepository tournamentRepository;

    @GetMapping("/tournament/allTournament")
    public String tournamentForm(Model model) {
        model.addAttribute("allTournament", tournamentRepository.findAll());
        return "tournament/allTournament";
    }

    @PostMapping("/tournament/allTournament")
    public ModelAndView goToCreate(ModelMap model) {
        return new ModelAndView("redirect:/tournament/create", model);
    }


    @PostMapping ("/tournament/currentTournament")
    public String currentTournament() {
        return "/";
    }
}

