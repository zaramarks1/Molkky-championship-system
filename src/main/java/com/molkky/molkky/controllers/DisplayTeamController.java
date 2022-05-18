package com.molkky.molkky.controllers;

import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/team")
public class DisplayTeamController {
    @Autowired
    TeamRepository teamRepository;

    @GetMapping("/displayTeams")
    public String displayTeams(Model model) {
        model.addAttribute("teams", teamRepository.findTeamsName());
        return "team/displayTeams";
    }

}



