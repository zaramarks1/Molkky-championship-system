package com.molkky.molkky.controllers;

import com.molkky.molkky.domain.Team;

import com.molkky.molkky.model.TeamFilterModel;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.servlet.ModelAndView;



@Controller
@RequestMapping("/team")
public class DisplayTeamController {
    @Autowired
    TeamRepository teamRepository;

    @GetMapping("/displayTeams")
    public String displayTeams(Model model) {
        model.addAttribute("teams", teamRepository.findAll());
        TeamFilterModel teamModel = new TeamFilterModel();
        model.addAttribute("teamModel", teamModel);
        Team team = new Team();
        model.addAttribute("team" ,team);
        return "team/displayTeams";
    }

    @PostMapping("/filter")
    public ModelAndView connexionUser(@ModelAttribute("teamModel")TeamFilterModel team2, Model model){
        if(Boolean.TRUE.equals(teamRepository.existsTeamByName(team2.getName()))){
            model.addAttribute("teams", teamRepository.findTeamByName(team2.getName()));
            return new ModelAndView("/team/displayTeams");
        }
        return new ModelAndView("/team/displayTeams");
    }

}



