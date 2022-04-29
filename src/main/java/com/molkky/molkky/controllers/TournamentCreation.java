package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class TournamentCreation {
    @Autowired
    private TournamentRepository tournamentRepository;

    @GetMapping("/tournament/create")
    public String tournamentForm(ModelMap model) {
        model.addAttribute("tournament", new TournamentModel());
        return "/tournament/create";
    }

    @PostMapping("/tournament/create")
    public ModelAndView tournamentSubmit(@ModelAttribute("tournament") TournamentModel tournament, ModelMap model) {
        Tournament tournamentEntity = tournamentRepository.save(new Tournament(tournament));
        model.addAttribute("tournament", tournamentEntity);
        return new ModelAndView( "redirect:/tournament/allTournament",model);
    }

}
