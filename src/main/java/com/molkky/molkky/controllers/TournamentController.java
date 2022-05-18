package com.molkky.molkky.controllers;


import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.TournamentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import type.TournamentStatus;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
@RequestMapping("/tournament")
public class TournamentController extends DefaultAttributes {
    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private TournamentService tournamentService;


    private String allTournament="tournament";
    private String redirectionAll = "tournament/allTournament";


    @GetMapping("/allTournament")
    public String tournamentForm(Model model) {
        model.addAttribute(allTournament, tournamentRepository.findAll());
        return redirectionAll;
    }

    @GetMapping("/TournamentOpen")
    public String tournamentOpen(Model model) {
        model.addAttribute(allTournament, tournamentRepository.findByVisibleAndStatus(true,TournamentStatus.AVAILABLE));
        return redirectionAll;
    }


    @GetMapping("/TournamentClose")
    public String tournamentClose(Model model) {
        model.addAttribute(allTournament, tournamentRepository.findByVisibleAndStatus(true,TournamentStatus.CLOSED));

        return redirectionAll;
    }

    @GetMapping("/TournamentInProgress")
    public String tournamentinProgress(Model model) {
        model.addAttribute(allTournament, tournamentRepository.findByVisibleAndStatus(true,TournamentStatus.INPROGRESS));
        return redirectionAll;
    }

    @PostMapping("/allTournament")
    public ModelAndView goToCreate(ModelMap model) {
        return new ModelAndView("redirect:/tournament/create", model);
    }


    @PostMapping ("/currentTournament")
    public String currentTournament() {
        return "/";
    }

    @GetMapping("/create")
    public String tournamentForm(Model model, HttpSession session) {
        model.addAttribute(allTournament, new TournamentModel());
        return "tournament/create";
    }



    @PostMapping("/create")
    public String tournamentSubmit(@Valid @ModelAttribute("tournament") TournamentModel tournament, Model model) {

        Tournament tournamentEntity = tournamentService.create(tournament);

        int id = tournamentEntity.getId();
        return "redirect:/phase/choosePhases?tournamentId="+id;
    }
    @GetMapping("/{id}/view")
    public String tournamentView(Model model, @PathVariable("id") String id){
        Tournament tournament = tournamentRepository.findById(Integer.valueOf(id));
        model.addAttribute("tournament", tournament);
        model.addAttribute(allTournament, tournament);
        model.addAttribute("nbTeam", tournament.getTeams().size());
        return "/tournament/view";
    }
    @GetMapping("/view")
    public String tournamentViewPostLaunch(Model model,@RequestParam(value = "tournamentId", required = false) String tournamentId){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setStatus(TournamentStatus.INPROGRESS);

        tournamentRepository.save(tournament);
        model.addAttribute("tournament",tournament);
        return "/tournament/view";
    }



}
