package com.molkky.molkky.controllers;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

@Controller
public class TournamentCreation {

    @GetMapping("/tournament/create")
    public String tournamentForm(Model model) {
        return "tournament/create";
    }

    @PostMapping("/tournament/create")
    public String tournamentSubmit(Model model) {
        return "tournament/create";
    }

}