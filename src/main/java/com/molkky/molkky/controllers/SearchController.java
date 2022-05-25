package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.TeamModel;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserModel;
import com.molkky.molkky.service.SearchService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@Controller
@RequestMapping("/search")
public class SearchController extends DefaultAttributes {
    @Autowired
    private SearchService searchService;

    @GetMapping
    @RequestMapping("/search")
    public String searchTournaments(Model model, @RequestParam(name = "term", required = true) String term){
        List<TournamentModel> tournaments = searchService.searchTournaments(term, 5);
        List<UserModel> users = searchService.searchUsers(term, 5);
        List<TeamModel> teams = searchService.searchTeams(term, 5);
        model.addAttribute("tournaments", tournaments);
        model.addAttribute("users", users);
        model.addAttribute("teams", teams);
        return "/fragments/searchAutoResults";
    }
}
