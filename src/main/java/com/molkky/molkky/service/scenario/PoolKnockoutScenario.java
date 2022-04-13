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
        generateMatchesForRound(tournament);
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

    public Knockout getKnockout(Tournament tournament){
        for(Round round : tournament.getRounds()){
            if(round.getType().equals("knockOut")){
                return round.getKnockout();
            }
        }
        return null;
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
            return getKnockout(tournament).getMatches();
        }
    }

    public Boolean areAllSwissPoolsFinished(Tournament tournament){
        for(SwissPool swissPool: getSwissPools(tournament)){
            if(Boolean.FALSE.equals(swissPool.getFinished())){
                logger.info("Pool non fini");
                return false;
            }
        }
        return true;
    }

    @Transactional
    public void setMatchScore(Match match, Integer scoreTeam1, Integer scoreTeam2, Tournament tournament){
//        set le score
        match.setScoreTeam1(scoreTeam1);
        match.setScoreTeam2(scoreTeam2);

//        check if match is finished
        if(match.getScoreTeam1() == 50 || match.getScoreTeam2() == 50){
            match.setFinished(true);

//            see who won
            if(match.getScoreTeam1() == 50){
                match.getTeams().get(0).setNbWins(match.getTeams().get(0).getNbWins() + 1);
                teamRepository.save(match.getTeams().get(0));
                logger.info("Team 1 a gagné");
            } else{
                match.getTeams().get(1).setNbWins(match.getTeams().get(1).getNbWins() + 1);
                teamRepository.save(match.getTeams().get(1));
                logger.info("Team 2 a gagné");
            }

//            knockout case, remove one team
            if(tournament.getIndexPhase() == 1) {
                getKnockout(tournament).setTeamsRemaining(getKnockout(tournament).getTeamsRemaining() - 1);
                knockoutRepository.save(getKnockout(tournament));
            }
            checkAndHandleIfMatchesAreFinished(tournament, match);
            matchRepository.save(match);
        }
    }

    @Transactional
    public void checkAndHandleIfMatchesAreFinished(Tournament tournament, Match match){
//        check if round is finished
        if(tournament.getIndexPhase() == 0){
//            swiss pool case
            if(Boolean.TRUE.equals(swissPoolService.areAllMatchesFinished(match.getSwissPool()))){
                logger.info("Tous les matchs de la pool sont finis");
                match.getSwissPool().setFinished(true);
                swissPoolRepository.save(match.getSwissPool());
                //            check if phase finished
                if(Boolean.TRUE.equals(areAllSwissPoolsFinished(tournament))){
                    logger.info("All swisspools finished");
                    goToNextPhase(tournament);
                }
            }
        } else {
            if(Boolean.TRUE.equals(knockoutService.areAllMatchesFinished(match.getKnockout()))){
                logger.info("Tous les matchs finis knockout");
//                a knockout is over when 1 team is remaining
                if(getKnockout(tournament).getTeamsRemaining() == 1){
                    getKnockout(tournament).setFinished(true);
                    knockoutRepository.save(getKnockout(tournament));
                    goToNextPhase(tournament);
                } else {
//                    if all matches are over but not 1 team is remaining, regenerate more matches
                    knockoutService.generateMatches(getKnockout(tournament), tournament, getWinningTeamsFromKnockout(tournament), getSwissPools(tournament).size());
                }
            }
        }
        matchRepository.save(match);
    }

    @Transactional
    public void goToNextPhase(Tournament tournament){
        if(tournament.getIndexPhase() == 0){
            tournament.setIndexPhase(tournament.getIndexPhase() + 1);
//        total divided by 4 bc of swissPools
            getKnockout(tournament).setTeamsRemaining(tournament.getTeams().size() / 2);
            getKnockout(tournament).getRound().setTeams(getWinningTeamsFromPool(tournament));
            tournamentRepository.save(tournament);

            generateMatchesForRound(tournament);
            resetWins(tournament);
//            reset wins for every team to handle knockout matches

        } else {
            logger.trace("Fin du tournoi");
            tournament.setFinished(true);
            tournamentRepository.save(tournament);
        }
    }

    @Transactional
    public void resetWins(Tournament tournament){
        for (Team team: tournament.getTeams()) {
            team.setNbWins(0);
            teamRepository.save(team);
        }
        
    }

//    generer les matchs pour la pool en cours
    public void generateMatchesForRound(Tournament tournament){
        List<Team> teams = new ArrayList<>(tournament.getTeams());
//        pour la pool il faut un match contre tout le monde
        if(tournament.getIndexPhase() == 0){
            for(int i = 0; i < getSwissPools(tournament).size(); i++){
                swissPoolService.generateMatches(getSwissPools(tournament).get(i), tournament, teams, i);
            }
            tournamentRepository.save(tournament);
        }
        else if(tournament.getIndexPhase() == 1){
            ArrayList<Team> winningTeams = new ArrayList<>(getWinningTeamsFromPool(tournament));
            knockoutService.generateMatches(getKnockout(tournament), tournament, winningTeams, getSwissPools(tournament).size());
        }
    }

    public List<Team> getWinningTeamsFromPool(Tournament tournament){
        ArrayList<Team> returnedTeams = new ArrayList<>();
        for(SwissPool swissPool: getSwissPools(tournament)){
            List<Team> teams = swissPool.getRound().getTeams();
            teams.sort(Comparator.comparing(Team::getNbWins));
            returnedTeams.add(teams.get(teams.size() - 1));
            returnedTeams.add(teams.get(teams.size() - 2));
        }
        return returnedTeams;
    }

    public List<Team> getWinningTeamsFromKnockout(Tournament tournament){
        ArrayList<Team> returnedTeams = new ArrayList<>();
        List<Team> teams = getKnockout(tournament).getRound().getTeams();
        teams.sort(Comparator.comparing(Team::getNbWins));
        for(int i = 0; i < teams.size(); i++){
            returnedTeams.add(teams.get(teams.size() - 1 - i));
        }
        return returnedTeams;
    }

}
