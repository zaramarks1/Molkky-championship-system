package com.molkky.molkky.controllers;


import com.molkky.molkky.controllers.superclass.DefaultAttributes;
import com.molkky.molkky.domain.Phase;
import com.molkky.molkky.domain.Team;
import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.rounds.*;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.UserLogged;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.service.TournamentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;
import type.PhaseType;
import type.PhaseTypeViewModel;
import type.TournamentStatus;
import type.UserRole;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

@Controller
@RequestMapping("/tournament")
public class TournamentController extends DefaultAttributes {
    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private TournamentService tournamentService;


    private String allTournament="tournament";
    private String redirectionAll = "/tournament/allTournament";


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

    @PostMapping("/inscription")
    public ModelAndView goToInscription(ModelMap model){return new ModelAndView("redirect:/team/create",model);}


    @PostMapping ("/currentTournament")
    public String currentTournament() {
        return "/";
    }

    @GetMapping ("/tournamentOnGoing")
    public String getTournamentOnGoing() {
        return "/tournament/tournamentOnGoing";
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

    @GetMapping("/view")
    public String tournamentViewPostLaunch(Model model,@RequestParam(value = "tournamentId", required = false) String tournamentId,  HttpSession session){

        
        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        //USER FROM SESSION
        UserLogged user = getUser(session);

        if (user != null) {
            if (user.getTournament().getId().toString().equals(tournamentId) && user.getRole().equals(UserRole.ADM)) {

                model.addAttribute("user", user);
            }else{
                model.addAttribute("user", null);
            }
        }


        List<Team> teams = tournament.getTeams();

        List<Phase> phases = tournament.getPhases();

        List<PhaseTypeViewModel> phasesType = new ArrayList<>();
        for (Phase p : phases) {
            if (p instanceof Pool) {
                Pool pool = (Pool) p;
                PhaseTypeViewModel phaseTypeViewModel = new PhaseTypeViewModel();
                phaseTypeViewModel.setPhase(pool);
                phaseTypeViewModel.setPhaseType(PhaseType.POOL);
                phasesType.add(phaseTypeViewModel);
            } else if (p instanceof SimpleGame) {
                SimpleGame pool = (SimpleGame) p;
                PhaseTypeViewModel phaseTypeViewModel = new PhaseTypeViewModel();
                phaseTypeViewModel.setPhase(pool);
                phaseTypeViewModel.setPhaseType(PhaseType.SIMPLEGAME);
                phasesType.add(phaseTypeViewModel);
            } else if (p instanceof Knockout) {
                Knockout pool = (Knockout) p;
                PhaseTypeViewModel phaseTypeViewModel = new PhaseTypeViewModel();
                phaseTypeViewModel.setPhase(pool);
                phaseTypeViewModel.setPhaseType(PhaseType.KNOCKOUT);
                phasesType.add(phaseTypeViewModel);
            } else if (p instanceof SwissPool) {
                SwissPool pool = (SwissPool) p;
                PhaseTypeViewModel phaseTypeViewModel = new PhaseTypeViewModel();
                phaseTypeViewModel.setPhase(pool);
                phaseTypeViewModel.setPhaseType(PhaseType.SWISSPOOL);
                phasesType.add(phaseTypeViewModel);
            } else if (p instanceof Finnish) {
                Finnish pool = (Finnish) p;
                PhaseTypeViewModel phaseTypeViewModel = new PhaseTypeViewModel();
                phaseTypeViewModel.setPhase(pool);
                phaseTypeViewModel.setPhaseType(PhaseType.FINNISH);
                phasesType.add(phaseTypeViewModel);
            }
        }

        model.addAttribute("tournament", tournament);
        model.addAttribute("phasesType", phasesType);
        model.addAttribute(allTournament, tournament);
        model.addAttribute("nbTeam", tournament.getTeams().size());
        return "/tournament/view";

    }

    @PostMapping("/addStaffMembers")
    public String addStaffToTournament(Model model, @RequestParam(name="staffCount") String staffCount,
                                       @RequestParam(name="tournamentId") String tournamentId) {

        model.addAttribute("tournament_id", tournamentId);
        model.addAttribute("staff_counter", staffCount);
        return "redirect:/tournament/"+tournamentId+"/view";
    }

    @PostMapping("/setVisible")
    public String setVisibleTournament(Model model,@RequestParam(name="tournamentId") String tournamentId ){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setVisible(true);
         tournamentRepository.save(tournament);

        model.addAttribute("tournament_id", tournamentId);

        return "redirect:/tournament/view?tournamentId=" + tournamentId;

    }

    @PostMapping("/publish")
    public String pubishTournament(Model model,@RequestParam(name="tournamentId") String tournamentId ){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setStatus(TournamentStatus.INPROGRESS);
        tournamentRepository.save(tournament);

        model.addAttribute("tournament_id", tournamentId);

        return "redirect:/tournament/view?tournamentId=" + tournamentId;

    }

}


