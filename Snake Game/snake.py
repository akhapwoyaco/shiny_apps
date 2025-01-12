# snake game using pygame
import pygame #this is our game module used for making 2D games
import random #for randomly placing food for snake
import sys #for exiting the game
import time #for sleeping

checkErrors  = pygame.init() #this returns the tuple with successfull execution and number orf errors occured
if checkErrors[1]>0:
	print("Some error occured {0}".format(checkErrors[1]))
	sys.exit(-1)z
else:
	print("Pygame initialized successfully")

# Play Surface / display
playSurface = pygame.display.set_mode((750,450))

pygame.display.set_caption('Snake Game!')

# Colors
red = pygame.Color(255,0,0) #r,g,b 
green = pygame.Color(0,255,0) #snake
black = pygame.Color(0,0,0) #score
white = pygame.Color(255,255,255) #background
brown = pygame.Color(165,42,42) #food

# Frames per second controller

fpscontroller = pygame.time.Clock()

# Variables
snakePos = [100,50]
snakeBody = [[100,50],[90,50],[80,50]]

foodPos = [random.randrange(1,75)*10,random.randrange(1,45)*10] #randomly generate position of food
foodStatus = True
score = 0 
direction = 'RIGHT'
changeTo = direction

def showScore(choice = 1):
	scoreFont = pygame.font.SysFont('monaco',24)
	scoreSurface = scoreFont.render('Score : {0}'.format(score),True,white)
	scoreRect = scoreSurface.get_rect()
	if choice == 1:
		scoreRect.midtop = (80,10)
	else:
		scoreRect.midtop = (360,120)
	playSurface.blit(scoreSurface,scoreRect)
	
#game over function
def gameOver():
	myFont = pygame.font.SysFont('monaco',72)
	GameOverSurvace = myFont.render('Game Over!',True,red)
	GameOverRect = GameOverSurvace.get_rect()
	GameOverRect.midtop = (360,15)
	playSurface.blit(GameOverSurvace,GameOverRect)
	
	showScore(0)
	pygame.display.update()
	time.sleep(2)
	pygame.quit() #pygame exit 
	sys.exit() # console exit


# Main logic of the game
while(True):
	for event in pygame.event.get():
		# exit alt f4
		if event.type == pygame.QUIT:
			pygame.quit()
			sys.exit()
		elif event.type == pygame.KEYDOWN:
			if event.key == pygame.K_RIGHT or event.key == ord('d'):
				changeTo = 'RIGHT'
			if event.key == pygame.K_LEFT or event.key == ord('a'):
				changeTo = 'LEFT'
			if event.key == pygame.K_UP or event.key == ord('w'):
				changeTo = 'UP'
			if event.key == pygame.K_DOWN or event.key == ord('s'):
				changeTo = 'DOWN'
			if event.key == pygame.K_ESCAPE:
				pygame.event.post(pygame.event.Event(pygame.QUIT))
	# validation of directions				
	if changeTo == 'RIGHT' and not direction =='LEFT':
		direction = 'RIGHT'
	if changeTo == 'LEFT' and not direction =='RIGHT':
		direction = 'LEFT'
	if changeTo == 'DOWN' and not direction =='UP':
		direction = 'DOWN'
	if changeTo == 'UP' and not direction =='DOWN':
		direction = 'UP'
	# change snake position
	if direction == 'RIGHT':
		snakePos[0] = snakePos[0] + 10
	if direction == 'LEFT':
		snakePos[0] = snakePos[0] - 10
	if direction == 'UP':
		snakePos[1] = snakePos[1] - 10
	if direction == 'DOWN':
		snakePos[1] = snakePos[1] + 10
	# update snake body
	snakeBody.insert(0,list(snakePos))
	if snakePos[0] == foodPos[0] and snakePos[1] == foodPos[1]:
		score+=1
		foodStatus = False
	else:
		snakeBody.pop()

	if foodStatus == False:
		foodPos = [random.randrange(1,75)*10,random.randrange(1,45)*10]
	foodStatus = True

	playSurface.fill(black)
	for pos in snakeBody:
		pygame.draw.rect(playSurface,green, pygame.Rect(pos[0],pos[1],10,10))

	# draw food
	pygame.draw.rect(playSurface,brown, pygame.Rect(foodPos[0],foodPos[1],10,10))
	
	# check boundary
	if snakePos[0]>740 or snakePos[0]<0:
		gameOver()
	elif snakePos[1]>440 or snakePos[1]<0:
		gameOver()
	for block in snakeBody[1:]:
		if snakePos[0]== block[0] and snakePos[1] ==block[1]:
			gameOver()

	
	
	showScore()
	pygame.display.update()
	fpscontroller.tick(50)



