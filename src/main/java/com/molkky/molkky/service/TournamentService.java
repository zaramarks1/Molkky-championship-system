package com.molkky.molkky.service;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.model.TournamentModel;
import com.molkky.molkky.model.phase.PhaseRankingModel;
import com.molkky.molkky.repository.TeamRepository;
import com.molkky.molkky.repository.TournamentRepository;
import com.molkky.molkky.repository.UserRepository;
import com.molkky.molkky.repository.UserTournamentRoleRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import type.TournamentStatus;
import type.UserRole;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static com.molkky.molkky.utility.StringUtilities.createCode;


@Service
public class TournamentService {

    @Autowired
    TournamentRepository tournamentRepository;

    @Autowired
    UserRepository userRepository;

    @Autowired
    UserTournamentRoleRepository userTournamentRoleRepository;

    @Autowired
    TeamRepository teamRepository;

    @Autowired
    RoundService roundService;

    public final Date currentDate = new Date();

    public Tournament create(TournamentModel tournamentModel) {
        Tournament tournament = new Tournament(tournamentModel);

        tournament = tournamentRepository.save(tournament);

        String mail = tournamentModel.getEmail();

        User user = new User();

        if (!userRepository.existsUserByEmail(mail)) {
            user.setEmail(mail);
            user.setPassword(createCode(5));
            user = userRepository.save(user);
        } else {
            user = userRepository.findUserByEmail(mail);
        }

        UserTournamentRole userTournamentRole = new UserTournamentRole();

        userTournamentRole.setUser(user);
        userTournamentRole.setRole(UserRole.ADM);
        userTournamentRole.setTournament(tournament);

        userTournamentRoleRepository.save(userTournamentRole);

        return tournament;
    }

    // Fonction qui test si le nombre d'équipes inscrites après la date limite d'inscription est suffisant. Sinon, ferme le tournament
    public void closeTournamentWhenMinimumTeamsBeforeDate() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            if (currentDate.after(tournament.getCutOffDate()) && tournament.getTeams().size() <= tournament.getMinTeam()) {
                tournament.setStatus(TournamentStatus.CLOSED);
            }
        }
    }

    // Test quand les inscriptions du tournoi doivent être fermées
    public void registerClosedForTournament() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            if (tournament.getStatus() == TournamentStatus.AVAILABLE && isTournamentNotAvailable(tournament)) {
                tournament.setRegisterAvailable(false);
                tournamentRepository.save(tournament);
            }
        }
    }

    private boolean isTournamentNotAvailable(Tournament tournament) {
        return new Date().after(tournament.getCutOffDate()) || tournament.getMaxTeam() == tournament.getTeams().size();
    }

    // Récupère le gagnant du tournoi
    // Format return list car possibilité qu'il y ait plusieurs gagnants pas écartée pour le moment
    public List<Team> getWinners(Tournament tournament){

        int nbPhases = tournament.getPhases().size()-1;
       // List<Team> teams = teamRepository.findByTournamentAndEliminated(tournament, false);
        List<Team> teams = new ArrayList<>();
        List<PhaseRankingModel> phaseRankingModels;
        for(int i = nbPhases; i >=0; i--){
            Phase phase = tournament.getPhases().get(i);
            phaseRankingModels = roundService.orderTeamsByScoreInPhase(phase, phase.getVictoryValue());

            for(PhaseRankingModel r : phaseRankingModels) if(!teams.contains(r.getTeam())) teams.add(r.getTeam());
        }
        return teams;
    }

    // Lance le tournoi si la date de ce dernier est celle du jour
    public void defineMatchInProgress() {
        List<Tournament> tournaments = tournamentRepository.findAll();

        for (Tournament tournament : tournaments) {
            boolean condition = currentDate.after(tournament.getDate());
            if (currentDate.after(tournament.getDate())) {
                tournament.setStatus(TournamentStatus.INPROGRESS);
                tournament.setRegisterAvailable(false);
            }
        }
    }
}
