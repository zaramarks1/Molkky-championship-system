package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.TeamFilterModel;
import com.molkky.molkky.repository.TeamRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;



@Controller
@RequestMapping("/team")
public class DisplayTeamController extends DefaultAttributes {
    @Autowired
    TeamRepository teamRepository;

    @GetMapping("/displayTeams")
    public String displayTeams(Model model, @RequestParam(value = "filter", required = false) String filter) {
        TeamFilterModel teamModel = new TeamFilterModel();
        model.addAttribute("teamModel", teamModel);
        if(filter != null && !"".equals(filter)){
            model.addAttribute("teams" , teamRepository.searchTeamsByName(filter, 10));
        } else {
            model.addAttribute("teams" , teamRepository.findAll());
        }
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



