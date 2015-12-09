#!/usr/bin/env python
from UserDict import UserDict
from UserList import UserList

import pandas
from numpy import random
import unipath
import yaml

from psychopy import prefs
try:
    import pyo
except ImportError:
    print 'pyo not found!'
prefs.general['audioLib'] = ['pyo', ]
from psychopy import visual, core, event, sound
sound.init(48000, buffer=128)

from labtools.psychopy_helper import get_subj_info, load_sounds, load_images
from labtools.dynamic_mask import DynamicMask
from labtools.trials_functions import (counterbalance, expand, extend,
                                       add_block, smart_shuffle)

class Participant(UserDict):
    """ Store participant data and provide helper functions.

    >>> participant = Participant(subj_id=100, seed=539,
                                  _order=['subj_id', 'seed'])
    >>> participants.data_file
    # data/100.csv
    >>> participant.write_header(['trial', 'is_correct'])
    # writes "subj_id,seed,trial,is_correct\n" to the data file
    # and saves input as the order of columns in the output
    >>> participant.write_trial({'trial': 1, 'is_correct': 1})
    # writes "100,539,1,1\n" to the data file
    """
    DATA_DIR = 'data'
    DATA_DELIMITER = ','

    def __init__(self, subj_id, **kwargs):
        """ Standard dict constructor.

        subj_id is required and is used in the creation of the data file.

        Saves _order if provided. Raises an AssertionError if _order
        isn't exhaustive of kwargs.
        """
        kwargs['subj_id'] = subj_id

        self._data_file = None
        self._order = kwargs.pop('_order', kwargs.keys())

        not_exhaustive_msg = "order not exhaustive of kwargs"
        assert set(self._order) == set(kwargs), not_exhaustive_msg

        self.data = dict(**kwargs)

    def keys(self):
        return self._order

    @property
    def data_file(self):
        if not unipath.Path(self.DATA_DIR).exists():
            unipath.Path(self.DATA_DIR).mkdir()

        if not self._data_file:
            data_file_name = '{subj_id}.csv'.format(**self)
            self._data_file = unipath.Path(self.DATA_DIR, data_file_name)
        return self._data_file

    def write_header(self, trial_col_names):
        """ Writes the names of the columns and saves the order. """
        self._col_names = self._order + trial_col_names
        self._write_line(self.DATA_DELIMITER.join(self._col_names))

    def write_trial(self, trial):
        assert self._col_names, 'write header first to save column order'
        trial_data = dict(self)
        trial_data.update(trial)
        row_data = [str(trial_data[key]) for key in self._col_names]
        self._write_line(self.DATA_DELIMITER.join(row_data))

    def _write_line(self, row):
        with open(self.data_file, 'a') as f:
            f.write(row + '\n')


class Trials(UserList):
    STIM_DIR = unipath.Path('stimuli')
    COLUMNS = [
        # Trial columns
        'block',
        'block_type',
        'trial',

        # Stimuli columns
        'proposition_id',
        'feat_type',
        'question',
        'cue',
        'cue_file',
        'mask_type',
        'response_type', # prompt or word
        'word',
        'word_file',
        'correct_response',

        # Response columns
        'response',
        'rt',
        'is_correct',
    ]

    @classmethod
    def make(cls, **kwargs):
        """ Create a list of trials.

        Each trial is a dict with values for all keys in self.COLUMNS.
        """
        stim_dir = unipath.Path(cls.STIM_DIR)

        # default settings
        settings = dict(ratio_prompt_trials=0.75,
                        ratio_yes_correct_responses=0.75)
        settings.update(kwargs)

        seed = kwargs.get('seed', '')
        try:
            seed = int(seed)
        except ValueError:
            seed = random.randint(100)
            print 'no seed given, using', seed

        prng = random.RandomState(seed)

        # Balance within subject variables
        trials = counterbalance({'feat_type': ['visual', 'nonvisual'],
                                 'mask_type': ['mask', 'nomask']})
        trials = expand(trials, name='correct_response', values=['yes', 'no'],
                        ratio=settings['ratio_yes_correct_responses'],
                        seed=seed)
        trials = expand(trials, name='response_type', values=['prompt', 'word'],
                        ratio=settings['ratio_prompt_trials'],
                        seed=seed)

        # Extend the trials to final length
        trials = extend(trials, reps=4)

        # Read proposition info
        propositions_csv = unipath.Path(stim_dir, 'propositions.csv')
        propositions = pandas.read_csv(propositions_csv)

        # Add cue
        categories = propositions.cue.unique()
        trials['cue'] = prng.choice(categories, len(trials), replace=True)

        cue_info_csv = unipath.Path(stim_dir, 'cues', '_cue_info.csv')
        cues = pandas.read_csv(cue_info_csv)

        def determine_cue_file(row):
            options = cues.ix[cues.cue == row['cue'], 'cue_file'].tolist()
            return prng.choice(options)

        trials['cue_file'] = trials.apply(determine_cue_file, axis=1)

        # copy propositions to prevent duplicate questions
        _propositions = propositions.copy()
        def determine_question(row):
            is_cue = (_propositions.cue == row['cue'])
            is_feat_type = (_propositions.feat_type == row['feat_type'])
            is_correct_response = (_propositions.correct_response ==
                                   row['correct_response'])

            valid_propositions = (is_cue & is_feat_type & is_correct_response)

            if valid_propositions.sum() == 0:
                # no more propositions for this cue!
                # switch to a different cue and try again
                # row.name is the index for this row
                trials.loc[row.name, 'cue'] = prng.choice(categories)
                trials.loc[row.name, 'cue_file'] = determine_cue_file(trials.ix[row.name])
                return determine_question(trials.ix[row.name, ])

            options = _propositions.ix[valid_propositions, ]
            selected_ix = prng.choice(options.index)
            selected_proposition_id = options.ix[selected_ix, 'proposition_id']
            _propositions.drop(selected_ix, inplace=True)

            return selected_proposition_id

        trials['proposition_id'] = trials.apply(determine_question, axis=1)

        # Merge in question
        trials = trials.merge(propositions)

        # Replace proposition on word trials
        trials.loc[trials.response_type == 'word', 'question'] = '----------'
        for prop_col in ['question_slug', 'feat_type', 'proposition_id']:
            trials.loc[trials.response_type == 'word', prop_col] = ''

        # Add in word
        def determine_word(row):
            if row['response_type'] != 'word':
                return ''
            elif row['correct_response'] == 'yes':
                return row['cue']
            else:
                distractors = list(categories)
                distractors.remove(row['cue'])
                return prng.choice(distractors)

        trials['word'] = trials.apply(determine_word, axis=1)

        # for V2, the word is now auditory, so a file must be selected
        def determine_word_file(row):
            if row['response_type'] == 'prompt':
                return ''
            options = cues.ix[cues.cue == row['word'], 'cue_file'].tolist()
            if row['cue_file'] in options:
                options.remove(row['cue_file'])
            return prng.choice(options)

        trials['word_file'] = trials.apply(determine_word_file, axis=1)

        # Add columns for response variables
        for col in ['response', 'rt', 'is_correct']:
            trials[col] = ''

        # Add practice trials
        num_practice = 8
        practice_ix = prng.choice(trials.index, num_practice)
        practice_trials = trials.ix[practice_ix, ]
        practice_trials['block'] = 0
        practice_trials['block_type'] = 'practice'
        trials.drop(practice_ix, inplace=True)

        # Separate response type by block
        # HARDCODED!!!
        prompt_blocks = [1, 3, 5]
        word_blocks = [2, 4]

        def assign_block(row):
            response_type = row['response_type']
            if response_type == 'prompt':
                return prng.choice(prompt_blocks)
            elif response_type == 'word':
                return prng.choice(word_blocks)
            else:
                raise NotImplementedError('response type %s' % response_type)

        trials['block'] = trials.apply(assign_block, axis=1)
        trials.sort('block', inplace=True)

        # Finishing touches
        trials = smart_shuffle(trials, col='cue', block='block', seed=seed)
        trials['block_type'] = 'test'

        # Merge practice trials
        trials = pandas.concat([practice_trials, trials])

        # Label trial
        trials['trial'] = range(len(trials))

        # Reorcder columns
        trials = trials[cls.COLUMNS]

        return cls(trials.to_dict('record'))

    @classmethod
    def load(cls, trials_csv):
        trials = pandas.read_csv(trials_csv)
        return cls(trials.to_dict('records'))

    def write_trials(self, trials_csv):
        trials = pandas.DataFrame.from_records(self)
        trials = trials[self.COLUMNS]
        trials.to_csv(trials_csv, index=False)

    def iter_blocks(self, key='block'):
        """ Yield blocks of trials. """
        block = self[0][key]
        trials_in_block = []
        for trial in self:
            if trial[key] == block:
                trials_in_block.append(trial)
            else:
                yield trials_in_block
                block = trial[key]
                trials_in_block = []


class Experiment(object):
    STIM_DIR = unipath.Path('stimuli')

    def __init__(self, settings_yaml, texts_yaml):
        with open(settings_yaml, 'r') as f:
            exp_info = yaml.load(f)

        self.waits = exp_info.pop('waits')
        self.response_keys = exp_info.pop('response_keys')
        self.survey_url = exp_info.pop('survey_url')

        with open(texts_yaml, 'r') as f:
            self.texts = yaml.load(f)

        self.win = visual.Window(fullscr=True, units='pix', allowGUI=False)

        text_kwargs = dict(win=self.win, height=30, font='Consolas',
                           color='black', wrapWidth=400)
        self.fix = visual.TextStim(text='+', **text_kwargs)
        self.question = visual.TextStim(**text_kwargs)
        self.prompt = visual.TextStim(text='?', **text_kwargs)
        self.word = visual.TextStim(**text_kwargs)

        self.cues = load_sounds(unipath.Path(self.STIM_DIR, 'cues'),
                                include_ext=True)  # key names strawberry_1.wav

        size = [400, 400]
        image_kwargs = dict(win=self.win, size=size)
        self.mask = DynamicMask(**image_kwargs)

        feedback_dir = unipath.Path(self.STIM_DIR, 'feedback')
        self.feedback = {}
        self.feedback[0] = sound.Sound(unipath.Path(feedback_dir, 'buzz.wav'))
        self.feedback[1] = sound.Sound(unipath.Path(feedback_dir, 'bleep.wav'))

        self.timer = core.Clock()

    def run_trial(self, trial):
        """ Run a trial using a dict of settings. """
        self.question.setText(trial['question'])

        cue = self.cues[trial['cue_file']]
        cue_dur = cue.getDuration()

        stim_during_cue = [self.fix, ]
        if trial['mask_type'] == 'mask':
            stim_during_cue.insert(0, self.mask)

        response_stim_sound = None
        if trial['response_type'] == 'word':
            response_stim_sound = self.cues[trial['word_file']]

        # Start trial presentation
        # ------------------------
        self.timer.reset()
        self.fix.draw()
        self.win.flip()
        core.wait(self.waits['fix_duration'])

        # Show the question
        self.timer.reset()
        self.question.draw()
        self.win.flip()
        core.wait(self.waits['question_duration'])

        # Delay between question offset and cue onset
        self.fix.draw()
        self.win.flip()
        core.wait(self.waits['question_offset_to_cue_onset'])

        # Play the cue
        self.timer.reset()
        cue.play()
        while self.timer.getTime() < cue_dur:
            [stim.draw() for stim in stim_during_cue]
            self.win.flip()
            core.wait(self.waits['mask_refresh'])

        # Show the response prompt
        self.timer.reset()
        self.prompt.draw()
        if response_stim_sound:
            response_stim_sound.play()
        self.win.flip()
        response = event.waitKeys(maxWait=self.waits['max_wait'],
                                  keyList=self.response_keys.keys(),
                                  timeStamped=self.timer)
        self.win.flip()
        # ----------------------
        # End trial presentation

        try:
            key, rt = response[0]
        except TypeError:
            rt = self.waits['max_wait']
            response = 'timeout'
        else:
            response = self.response_keys[key]

        is_correct = int(response == trial['correct_response'])

        trial['response'] = response
        trial['rt'] = rt * 1000
        trial['is_correct'] = is_correct

        if trial['block_type'] == 'practice' or trial['response_type'] == 'word':
            self.feedback[is_correct].play()

        if response == 'timeout':
            self.show_timeout_screen()

        core.wait(self.waits['iti'])

        return trial

    def show_instructions(self):
        introduction = sorted(self.texts['introduction'].items())

        text_kwargs = dict(win=self.win, wrapWidth=1000, color='black', font='Consolas')
        main = visual.TextStim(pos=[0, 200], **text_kwargs)
        example = visual.TextStim(pos=[0, -50], **text_kwargs)
        example.setHeight(30)

        for i, block in introduction:
            # For logic continent on block kwargs:
            tag = block.pop('tag', None)
            example_txt = block.pop('example', None)
            advance_keys = [block.get('advance', 'space'), 'q']

            # Draw main
            main.setText(block['main'])
            if tag == 'title':
                main.setHeight(50)
            else:
                main.setHeight(20)
            main.draw()

            # Draw example
            if example_txt:
                example.setText(example_txt)
                example.draw()

            if tag == 'mask':
                img_path = str(unipath.Path('labtools', 'dynamic_mask', 'colored_1.png'))
                mask = visual.ImageStim(self.win, img_path, pos=[0, -100])
                mask.draw()

            self.win.flip()
            key = event.waitKeys(keyList=advance_keys)[0]

            if key == 'q':
                core.quit()

            if key in ['up', 'down']:
                self.feedback[1].play()

    def show_end_of_practice_screen(self):
        visual.TextStim(self.win, text=self.texts['end_of_practice'],
                        height=30, wrapWidth=600, color='black',
                        font='Consolas').draw()
        self.win.flip()
        event.waitKeys(keyList=['space', ])

    def show_timeout_screen(self):
        visual.TextStim(self.win, text=self.texts['timeout'],
                        height=30, wrapWidth=600, color='black',
                        font='Consolas').draw()
        self.win.flip()
        event.waitKeys(keyList=['space', ])

    def show_break_screen(self):
        visual.TextStim(self.win, text=self.texts['break_screen'],
                        height=30, wrapWidth=600, color='black',
                        font='Consolas').draw()
        self.win.flip()
        event.waitKeys(keyList=['space', ])

    def show_end_of_experiment_screen(self):
        visual.TextStim(self.win, text=self.texts['end_of_experiment'],
                        height=30, wrapWidth=600, color='black',
                        font='Consolas').draw()
        self.win.flip()
        event.waitKeys(keyList=['space', ])

def make_experiment():
    return Experiment('settings.yaml', 'texts.yaml')

def main():
    participant_data = get_subj_info(
        'gui.yaml',
        # check_exists is a simple function to determine if the data file
        # exists, provided subj_info data. Here it's used to check for
        # uniqueness in subj_ids when getting info from gui.
        check_exists=lambda subj_info:
            Participant(**subj_info).data_file.exists()
    )

    participant = Participant(**participant_data)
    trials = Trials.make(**participant)

    # Start of experiment
    experiment = make_experiment()
    experiment.show_instructions()

    participant.write_header(trials.COLUMNS)

    last_block = trials[-1]['block']

    for block in trials.iter_blocks():
        block_num = block[0]['block']
        block_type = block[0]['block_type']

        print 'block_num', block_num
        print 'last_block', last_block
        print 'block_type', block_type

        for trial in block:
            trial_data = experiment.run_trial(trial)
            participant.write_trial(trial_data)

        if block_type == 'practice':
            experiment.show_end_of_practice_screen()
        elif block_num != last_block:
            experiment.show_break_screen()

    experiment.show_end_of_experiment_screen()

    import webbrowser
    webbrowser.open(experiment.survey_url.format(**participant))

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'command',
        choices=['run', 'trial', 'make', 'texts', 'survey'],
        nargs = '?', default = 'run'
    )
    parser.add_argument('--output', '-o', help='Name of output file')
    parser.add_argument('--seed', '-s', help='Seed')
    parser.add_argument('--trial-index', '-i', default=0, type=int,
                        help='Trial index to run from sample_trials.csv')

    args = parser.parse_args()

    if args.command == 'make':
        seed = args.seed or random.randint(100)
        output = args.output or 'sample_trials.csv'
        print "Making trials with seed %s: %s" % (seed, output)
        trials = Trials.make(seed=seed)
        trials.write_trials(args.output or 'sample_trials.csv')
    elif args.command == 'trial':
        experiment = make_experiment()
        trials = Trials.load('sample_trials.csv')
        experiment.run_trial(trials[args.trial_index])
    elif args.command == 'texts':
        experiment = make_experiment()
        experiment.show_instructions()
    elif args.command == 'survey':
        experiment = make_experiment()
        import webbrowser
        webbrowser.open(experiment.survey_url.format(subj_id='test-subj', computer='test-computer'))
    else:
        main()
