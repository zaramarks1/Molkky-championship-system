package com.molkky.molkky.controllers;


import com.molkky.molkky.domain.Tournament;
import com.molkky.molkky.domain.User;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.service.TounamentService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import type.TournamentStatus;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
@RequestMapping("/tournament")
public class TournamentController {
    @Autowired
    private TournamentRepository tournamentRepository;

    @Autowired
    private TounamentService tournamentService;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/create")
    public String tournamentForm(Model model, HttpSession session) {
        model.addAttribute("tournament", new TournamentModel());
        User user = (User)session.getAttribute("user");
        model.addAttribute("user", user);
        return "tournament/create";
    }

    @PostMapping("/create")
    public String tournamentSubmit(@Valid @ModelAttribute("tournament") TournamentModel tournament, Model model) {

        Tournament tournamentEntity = tournamentService.create(tournament);

        int id = tournamentEntity.getId();
        return "redirect:/tournament/"+id+"/view";
    }

    @GetMapping("/{id}/view")
    public String tournamentView(Model model, @PathVariable("id") String id){

        //USER FROM SESSION
        User user = null;

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(id));
        model.addAttribute("tournament", tournament);
        model.addAttribute("user", user);
        model.addAttribute("nbTeam", tournament.getTeams().size());

        return "tournament/view";
    }



    @PostMapping(value = "/view" , params = "launch")
    public String tournamentViewPostLaunch(@RequestParam(value = "tournamentId", required = false) String tournamentId){

        Tournament tournament = tournamentRepository.findById(Integer.valueOf(tournamentId));

        tournament.setStatus(TournamentStatus.INPROGRESS);

        tournamentRepository.save(tournament);

        return "redirect:/tournament/create";
    }



}
    @GetMapping("/tournament/allTournament")
    public String tournamentForm(Model model) {
        model.addAttribute("allTournament", tournamentRepository.findAll());
        return "tournament/allTournament";
    }

    @PostMapping("/tournament/allTournament")
    public ModelAndView goToCreate(ModelMap model) {
        return new ModelAndView("redirect:/tournament/create", model);
    }


    @PostMapping ("/tournament/currentTournament")
    public String currentTournament() {
        return "/";
    }
}

