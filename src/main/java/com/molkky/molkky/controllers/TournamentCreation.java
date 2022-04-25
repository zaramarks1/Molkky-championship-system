package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;

import javax.servlet.http.HttpSession;

@Controller
public class TournamentCreation {
    @Autowired
    private TournamentRepository tournamentRepository;

    @GetMapping("/tournament/create")
    public String tournamentForm(Model model, HttpSession session) {
        model.addAttribute("tournament", new TournamentModel());
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);
        return "tournament/create";
    }

    @PostMapping("/tournament/create")
    public String tournamentSubmit(@ModelAttribute("tournament") TournamentModel tournament, Model model) {

        Tournament tournamentEntity = tournamentRepository.save(new Tournament(tournament));
        
        model.addAttribute("tournament", tournamentEntity);
        return "tournament/create";
    }

}