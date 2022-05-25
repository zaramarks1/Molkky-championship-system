package com.molkky.molkky.controllers;

import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.service.SearchService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import type.SearchType;

import java.util.List;

@Controller
@RequestMapping("/search")
public class SearchController extends DefaultAttributes {
    @Autowired
    private SearchService searchService;

    @GetMapping
    @RequestMapping("/searchTournaments")
    public String searchTournaments(Model model, @RequestParam(name = "term", required = true) String term){
        List<TournamentModel> tournaments = searchService.searchTournaments(term);
        model.addAttribute("tournaments", tournaments);
        model.addAttribute("searchType", SearchType.TOURNAMENT);
        return "/fragments/searchAutoResults";
    }
}
