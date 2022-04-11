package com.molkky.molkky.service.scenario;

import com.molkky.molkky.domain.*;
import com.molkky.molkky.repository.*;
import com.molkky.molkky.service.pool.KnockoutService;
import com.molkky.molkky.service.pool.SwissPoolService;
import lombok.Data;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Data
@Service
public class PoolKnockoutScenario {
    private static final Logger logger = LoggerFactory.getLogger(PoolKnockoutScenario.class);

    private TournamentRepository tournamentRepository;
    private RoundRepository roundRepository;
    private SwissPoolRepository swissPoolRepository;
    private KnockoutRepository knockoutRepository;
    private MatchRepository matchRepository;
    private TeamRepository teamRepository;
    private SwissPoolService swissPoolService;
    private KnockoutService knockoutService;

    public PoolKnockoutScenario(TournamentRepository tournamentRepository, RoundRepository roundRepository, SwissPoolRepository swissPoolRepository, KnockoutRepository knockoutRepository, MatchRepository matchRepository, TeamRepository teamRepository, SwissPoolService swissPoolService, KnockoutService knockoutService) {
        this.tournamentRepository = tournamentRepository;
        this.roundRepository = roundRepository;
        this.swissPoolRepository = swissPoolRepository;
        this.knockoutRepository = knockoutRepository;
        this.matchRepository = matchRepository;
        this.teamRepository = teamRepository;
        this.swissPoolService = swissPoolService;
        this.knockoutService = knockoutService;
    }

    @Transactional
    public Tournament create(Tournament tournament){
        tournamentRepository.save(tournament);
//        creer les rounds
        generateRounds(tournament);

        logger.trace("Scénario initié");
        return tournamentRepository.save(tournament);
    }

    public void generateRounds(Tournament tournament){
        List<Round> roundsTournament = new ArrayList<>();
        Integer numberOfSwissPools = tournament.getTeams().size() / 4;
//        creer les rounds swiss
        for(int i = 0; i < numberOfSwissPools; i++){
            List<Team> teams = new ArrayList<>();
//            récuperer les 4 premieres teams
//            ex: 1 + 1 * 4 = 5
            teams.add(tournament.getTeams().get(i * 4));
            teams.add(tournament.getTeams().get(1 + i * 4));
            teams.add(tournament.getTeams().get(2 + i * 4));
            teams.add(tournament.getTeams().get(3 + i * 4));
            SwissPool swissPool = new SwissPool();
            Round swissRound = new Round();
            swissRound.setType("swissPool");
            swissRound.setNbTeams(4);
            swissRound.setTeams(teams);
            swissRound.setTournament(tournament);
            roundRepository.save(swissRound);
            swissPool.setRound(swissRound);
            swissRound.setSwissPool(swissPool);
            swissPoolRepository.save(swissPool);
            roundsTournament.add(swissRound);
        }
//        creer le round knockout
        Knockout knockout = new Knockout();

        Round knockOutRound = new Round();
        knockOutRound.setType("knockOut");
        knockOutRound.setNbTeams(tournament.getTeams().size() / 2);
        knockOutRound.setTournament(tournament);
        roundRepository.save(knockOutRound);
        knockout.setRound(knockOutRound);

        knockOutRound.setKnockout(knockout);
        knockoutRepository.save(knockout);
        roundsTournament.add(knockOutRound);
        tournament.setRounds(roundsTournament);
        tournamentRepository.save(tournament);
    }

    public void start(Tournament tournament){
        logger.trace("Scénario démarré");
        generateMatchesForPool(tournament);
    }

    public List<SwissPool> getSwissPools(Tournament tournament){
        List<SwissPool> swissPools = new ArrayList<>();
        for(Round round : tournament.getRounds()){
            if(round.getType().equals("swissPool")){
                swissPools.add(round.getSwissPool());
            }
        }
        return swissPools;
    }

//    Permet de recuperer les matchs de la phase actuel,
//    un poolknockout est fait de pools pouis d'un knock out
//    donc il recupere celui des pools d'abord
    public List<Match> getCurrentPhaseMatches(Tournament tournament){
        if(tournament.getIndexPhase() == 0){
//            cas encore dans la pool
            List<Match> matches = new ArrayList<>();
            for(SwissPool swissPool: getSwissPools(tournament)){
                matches.addAll(swissPool.getMatches());
            }
            return matches;
        } else{
//            cas dans le knock out
            return tournament.getRounds().get(1).getKnockout().getMatches();
        }
    }

    @Transactional
    public void setMatchScore(Match match, Integer scoreTeam1, Integer scoreTeam2, Tournament tournament){
        match.setScoreTeam1(scoreTeam1);
        match.setScoreTeam2(scoreTeam2);
        if(match.getScoreTeam1() == 50 || match.getScoreTeam2() == 50){
            match.setFinished(true);
            if(match.getScoreTeam1() == 50){
                match.getTeams().get(0).setNbWins(match.getTeams().get(0).getNbWins() + 1);
                teamRepository.save(match.getTeams().get(0));
            } else{
                match.getTeams().get(1).setNbWins(match.getTeams().get(1).getNbWins() + 1);
                teamRepository.save(match.getTeams().get(1));
            }
            matchRepository.save(match);
        }
        if(Boolean.TRUE.equals(swissPoolService.areAllMatchesFinished(match.getSwissPool()))){
            match.getSwissPool().setFinished(true);
            swissPoolRepository.save(match.getSwissPool());
            goToNextPhase(tournament);
        }
    }

    @Transactional
    public void goToNextPhase(Tournament tournament){
        if(tournament.getIndexPhase() == 0){
            tournament.setIndexPhase(tournament.getIndexPhase() + 1);
//        only two teams go into the knockout
            tournament.getRounds().get(1).getKnockout().setTeamsRemaining(2);
            tournamentRepository.save(tournament);

            generateMatchesForPool(tournament);
        } else {
            logger.trace("Fin du tournoi");
            tournament.setFinished(true);
            tournamentRepository.save(tournament);
        }
    }

//    generer les matchs pour la pool en cours
    public void generateMatchesForPool(Tournament tournament){
        List<Team> teams = new ArrayList<>(tournament.getTeams());
//        pour la pool il faut un match contre tout le monde
        if(tournament.getIndexPhase() == 0){
            for(SwissPool swissPool: getSwissPools(tournament)){
                swissPoolService.generateMatches(swissPool, tournament, teams);
            }
        }
        else if(tournament.getIndexPhase() == 1){
            ArrayList<Team> winningTeams = new ArrayList<>(getWinningTeamsFromPool(tournament));
            knockoutService.generateMatches(tournament.getRounds().get(1).getKnockout(), tournament, winningTeams);
        }
    }

    public List<Team> getWinningTeamsFromPool(Tournament tournament){
        List<Team> teams = new ArrayList<>(tournament.getTeams());
        teams.sort(Comparator.comparing(Team::getNbWins));
        ArrayList<Team> returnedTeams = new ArrayList<>();
        returnedTeams.add(teams.get(teams.size() - 1));
        returnedTeams.add(teams.get(teams.size() - 2));
        return returnedTeams;
    }

}
